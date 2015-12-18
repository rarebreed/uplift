(ns uplift.core
  (:require [taoensso.timbre :as timbre]
            [commando.command :as cmdr :refer [launch ssh which]]
            [commando.protos.protos :as protos :refer [output]]
            [uplift.utils.file-sys :as file-sys]
            [uplift.utils.algos :refer [lmap items varargs keywordize]]
            [uplift.config.reader :as ucr]
            [cheshire.core :as ches]
            clojure.string)
  (:import [java.nio.file Paths]
           [java.io File]
           [java.lang.management ManagementFactory]))

;; -----------------------------------------------------------------
(def osb (ManagementFactory/getOperatingSystemMXBean))
(def ddnsname (atom ""))
(def ddnshash (atom ""))
(def config (ucr/get-configuration))

(defrecord Distro
  [name
   version
   hostname
   hash
   timectl
   firewallctl
   repos
   ]
  )

(defn set-hostname
  [name]
  (reset! ddnsname name))

(defn set-ddnshash
  [hash]
  (reset! ddnshash hash))

(defn set-env
  [])

(defn copy-ssh-key
  "Copies the ssh public key to remote host.  Uses sshpass to get around prompting for password

  Args
  - host: hostname or IP address
  - pass-file: path to the password file
  - username: user to authorize as (default is root)
  - key-path: the public key to use (default what is in ~/.ssh/id_dsa.pub)
  "
  [^String host & {:keys [username key-path]
                   :or {username "root"
                        key-path (get-in config [:config :ssh-pub-key])}}]
  {:pre  [#(not (nil? (which "ssh-pass")))
          #(not (nil? (get-in config [:config :ssh-password])))
          #(file-sys/file-exists? key-path)]
   :post [#(= 0 (:exit %)) #(= 0 (-> (ssh host "ls") :exit))]}
  (let [deps (which "sshpass")
        sshpass-fmt "sshpass -e ssh-copy-id -i %s -o StrictHostKeyChecking=no %s@%s"
        base (format sshpass-fmt key-path username host)
        env {"SSHPASS" (get-in config [:config :ssh-password])}
        call (delay (launch base :env env))]
    (if (nil? deps)
      (let [sshpass (launch "yum install -y sshpass")]
        (if (not= 0 (:exit sshpass))
          (throw (Exception. "Unable to install sshpass to copy ssh public key"))
          (force call)))
      (force call))))


;; Ughhh, Java doesn't have a good way to get distro information.  So
;; we will scrape it from /etc/os-release
(defn distro-info
  "Runs lsb_release -a and gets the name, version, and variant type:
   {:distributor-id name of the distro (eg Fedora or RHEL)
    :release version number (eg 22 or 7.2
    :variant type of OS (eg workstation or server)}"
  ([info]
   (let [pattern "^%s:\\s*(.*)$"
         variant-patt #"\w+(Server|Client|Workstation)\w*"
         ;; Create parsers to match NAME, VERSION_ID and VARIANT_ID
         parsers (for [x ["Distributor ID" "Release"]]
                   {(keywordize x) (re-pattern (format pattern x))})
         ;; Run each regex on each line, return ([:NAME match?])
         matches (for [parser parsers
                       line (clojure.string/split info #"\n")]
                   (let [keyname (first (keys parser))
                         val (first (vals parser))
                         _ (timbre/debug "Testing: " line " with " val)]
                     [keyname (re-find val line)]))
         ;; Only get the elements in the seq where the second element isn't nil
         filtered (filter (fn [%] (if (second %) true nil)) matches)
         ;; passed to reduce to return our final map
         finalfn (fn [coll entry]
                   (merge coll (let [f (first entry)
                                     [whole value] (second entry)]
                                 (hash-map f value))))
         m (reduce finalfn {} filtered)
         variant (second (re-find variant-patt (:distributor-id m)))]
     (assoc m :variant variant)))
  ([]
   (let [res (launch "lsb_release -a")
         output (:output res)]
     (distro-info output))))


(defn remote-distro-info
  [host]
  (let [rfile (launch "lsb_release -a" :host host :throws? true)]
    (distro-info (:output rfile))))


(def yum-script
  ["import yum"
   "import json"
   "yb = yum.YumBase()"
   "yb_s = json.dumps(yb.conf.yumvar)"
   "print yb_s"])


(defn yum-base
  "Copies python script to remote host and executes it"
  [host]
  (spit "yum_base.py" (clojure.string/join "\n" yum-script))
  (file-sys/send-file-to host "yum_base.py")
  (let [result (launch "python yum_base.py" :host host)
        output (second (clojure.string/split (:output result) #"\n"))
        yum (ches/parse-string output)]
    yum))


(defn enabled-repos
  [& {:keys [host]}]
  (let [enabled (launch "yum repolist enabled" :host host)]
    enabled))

;; Add a pre hook to verify that repo file has been installed
(defn install-devtools
  [host]
  (launch "yum groupinstall -y \"Development Tools\"" :host host))


(defn git-clone
  "Clones a git repo onto host"
  [host url & {:keys [dir]}]
  (let [udir (when-not dir
               (let [udir (get-in (ucr/get-configuration) [:config :uplift-dir])]
                 (.getParent (File. udir))))
        dir (if dir dir udir)]
    (launch (format "cd %s; git clone %s" dir url) :host host)))


;; TODO:  Test this
(defn git-clone-pull
  [repo]
  (let [f (-> (Paths/get repo (into-array String [])) .toFile)
        localfn #(for [cmd ["git clean -dxf" "git pull"]]
                  (launch cmd {:dir %}))
        remotefn #(launch (str "git clone " %))]
    (if (.isDirectory f)
      (localfn repo)
      (remotefn repo))))


(defn get-arch
  "Gets the JVM's arch that it is running on.  Note that it returns 
   amd64 (which is the real name, not x86_64) for 64bit on x86"
  []
  (.getArch osb))


;; We will install the openjdk packages including jre and jdk on remote system
(defn install-jdk
  "Will install the Java openjdk on the remote system"
  [host version]
  ;; This assumes we have openjdk in our repos
  (let [jdks (map #(format % version) ["java-1.%d.0-openjdk-devel"
                                       "java-1.%d.0-openjdk"])
        install (map #(launch (str "yum install -y " %) :host host) jdks)]
    install))


(defn remote-download
  "ssh'es into remote host and curl the file to dest path on remote host"
  [host url dest]
  (let [cmd (format "curl %s -o %s" url dest)]
    (launch cmd :host host)))


(defmacro wrap
  "Takes a function call and surrounds it with a try catch.  Logs the function name
   the args supplied to the function "
  [head]
  `(let [fnname# (first '~head)
         args# (rest (list ~@head))]
     (timbre/info "evaluating function:" fnname# ", args:" args#)
     (try
       ~head
       (catch Exception ex#
         {:name fnname# :args args# :ex ex#}))))


;; TODO make this take pairs where the first element of the pair is the function to be called
;; and the second is a sequence of exception handling functions eg
;; (try+
;;   [(/ 1 0) [ArithmeticException some-handler args]]
;;   [(send-url "foo.com") [NetworkException net-handler args]])
(defmacro try+
  ([head]
   [`(wrap ~head)])
  ([head & tail]
   `(lazy-seq
      (cons
        (wrap ~head)
        (try+ ~@tail)))))


(defn check-java
  "Returns"
  [& {:keys [host]}]
  (let [version (launch "java -version" :host host)
        outp (:output version)]
    (if (not= 0 (:status version))
      ["No java installed" "0" "0"]
      (first (re-seq #"\d\.(\d)\.\d_(\d{2})" outp)))))


(defn- check-results
  [results]
  (letfn [(all-zero
            [acc new]
            (if (and acc (= 0 (:exit new)))
              true
              (reduced false)))]
    (and (reduce all-zero true (:results results))
         (not (:exceptions? results)))))


(defn install-lein
  "Installs leiningen on the remote host and puts it in /usr/local/bin"
  [host dest]
  ;{:post [(check-results %)]}
  (let [lein-url "https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein"
        edit (fn []
               (let [bashrc (file-sys/get-remote-file host "~/.bashrc")
                     contents (when (= 0 (:status bashrc))
                                (slurp ".bashrc"))
                     edited (if contents
                              (str contents "\nexport LEIN_ROOT=1\n")
                              (throw (RuntimeException. "could not get .bashrc file")))
                     _ (spit "/tmp/.bashrc" edited)]
                 (file-sys/send-file-to host "/tmp/.bashrc" :dest "~/.bashrc")))
        results (try+
                  (remote-download host lein-url dest)
                  (launch (format "chmod ug+x %s" dest) :host host)
                  (edit)
                  (launch "lein" :host host :env {"LEIN_ROOT" "1"}))
        final {:results results :exceptions? (some #(instance? Exception %) results)}]
    final))


(defn lein-self-update
  "Calls lein upgrade"
  []
  @(launch "lein upgrade"))


(defn setup-system-time
  "Runs ntpd on system"
  [])


(defn reductor
  "Helps with the common scenario of changing a map based on items in a
   sequence"
  [m entry]
  (let [[k v] entry]
    (assoc m k v)))


(defn install-redhat-ddns
  "Installs the redhat-ddns-client

  This should be called after the repo files have been installed."
  [& {:keys [host url]}]
  (let [url-path (if (empty? url)
                   (get-in config [:config :ddns-client])
                   url)]
    (launch (str "rpm -Uvh " url-path) :host host)))


(defn install-vm
  "Uses virt-install to install a virtual machine"
  [location]
  (let [cmd "virt-install -l %s "])
  )


(defn create-dogtail-repo
  "Creates the dogtail repo to install dogtail"
  [])
