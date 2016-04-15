(ns uplift.core
  (:require [taoensso.timbre :as timbre]
            [commando.command :as cmdr :refer [launch ssh which]]
            [commando.protos.protos :as protos :refer [output]]
            [uplift.utils.file-sys :as file-sys]
            [uplift.utils.algos :refer [lmap items varargs keywordize]]
            [uplift.config.reader :as ucr]
            [cheshire.core :as ches]
            [uplift.utils.log-config :as lc]
            [uplift.protos :as uprotos :refer [RepoManager SystemSetup]]
            [clojure.core.match :refer [match]]
            clojure.string)
  (:import [java.nio.file Paths]
           [java.io File]
           [java.lang.management ManagementFactory]))

;; -----------------------------------------------------------------
(def osb (ManagementFactory/getOperatingSystemMXBean))
(def config (ucr/get-configuration))

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


(defn get-arch
  [host]
  (let [output (-> (cmdr/launch "uname -m" :host host) :output)
        arch (re-find #"i[3456]86|x86_64" output)]
    arch))

(defn assert-arch
  [host]
  (let [arch (get-arch host)]
    (if arch true false)))


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

;; FIXME: not needed...use setup-system
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

  *Precondition*: This should be called after the repo files have been installed."
  [& {:keys [host url]
      :or {url (get-in config [:config :ddns-client])}}]
  (launch (str "rpm -Uvh " url) :host host))


;; TODO: use selenium to setup a new host
(defn ddns-host-create
  "Uses selenium to setup a new ddns host"
  [user pw hostname]
  (let [url (format "https://%s:%s@")])
  )

;; TODO: use selenium to delete a host
(defn ddns-host-delete
  [user pw hostname])

(defn edit-ddns-hosts
  "Edits the /etc/redhat-ddns/hosts file"
  [host testname uuid & {:keys [hostfile]
                         :or {hostfile "/etc/redhat-ddns/hosts"}}]
  (let [ddns-file (file-sys/get-remote-file host hostfile)
        contents (slurp ddns-file)
        search-patt (re-pattern uuid)
        newline (clojure.string/join " " [testname "usersys.redhat.com" uuid])]
    (when-not (re-seq search-patt contents)
      (spit ddns-file newline :append true)
      (file-sys/send-file-to host ddns-file :dest hostfile))))

(defn ddns-client-enable
  [host]
  (cmdr/launch "/usr/bin/redhat-ddns-client enable" :host host)
  (cmdr/launch "/usr/bin/redhat-ddns-client" :host host))


(defn add-alias
  [cmdname cmd & {:keys [bashrc host]
                  :or {host "localhost"}}]
  (let [bashrc (if bashrc
                 bashrc
                 (let [home (-> (launch "echo $HOME" :host host) :output (clojure.string/trim))]
                   (file-sys/path-join home ".bashrc")))
        alias- (format "\nalias %s='%s'" cmdname cmd)
        oldbashrc (file-sys/get-remote-file host bashrc)
        backup (file-sys/make-backup bashrc :host host)]
    (spit oldbashrc alias- :append true)
    {:send-result (file-sys/send-file-to host oldbashrc :dest bashrc)
     :bashrc bashrc
     :alias alias-
     :oldbashrc oldbashrc
     :backup backup}))

(defn query-pkg
  [host pkgname]
  (:output (launch (str "rpm -q " pkgname) :host host :throws? false)))

(defn list-packages
  [& {:keys [host]}]
  (-> (launch  "rpm -qa" :host host :show-out? false) :output (clojure.string/split #"\n")))

(defn filter-packages
  [packages re]
  (let [re (cond
             (= String (type re)) (re-pattern re)
             (= java.util.regex.Pattern (type re)) re
             :else (throw (Exception. "re arg must be a string or regex pattern")))]
    (filter #(re-find re %) packages)))


(defn- check-deps
  [host deps]
  (every? #(if % % false)
          (for [d deps]
            (-> (launch (str "rpm -q " d) :host host) :status (= 0)))))

(defn install-deps
  [deps & {:keys [host]}]
  {:post [(partial check-deps host)]}
  (let [yum-deps (clojure.string/join " " deps)
        yum-cmd (format "yum install -y %s" yum-deps)]
    (launch yum-cmd :host host :show-out? false)))


(defn yum-whatprovides
  [name & {:keys [host]}]
  (launch "yum check-update" :host host :throws false)
  (let [pkg (launch (str "yum whatprovides " name) :host host :throws false)
        pkg (if (not= 0 (:status pkg))
              (launch (format "yum whatprovides */%s" name) :host host :throws false)
              pkg)]
    pkg))


(defn package-installed?
  "Predicate to check if a yum package was installed"
  [pkgname & {:keys [host]}]
  (let [cmd (format "rpm -q %s" pkgname)
        result (launch cmd :host host :throws? false)
        status (:status result)]
    (= 0 status)))
