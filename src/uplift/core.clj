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
(def ddnsname (atom ""))
(def ddnshash (atom ""))
(def config (ucr/get-configuration))

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

(defrecord Distro
  [distributor-id
   release
   variant
   major
   minor
   arch])


(def yum-script
  ["import yum"
   "import json"
   "import platform"
   "import copy"
   "yb = yum.YumBase()"
   "d_id, release, _ = platform.linux_distribution()"
   "temp = copy.deepcopy(yb.conf.yumvar)"
   "temp['release'] = release"
   "temp['id'] = d_id"
   "yb_s = json.dumps(temp)"
   "print yb_s"])


(defn yum-base
  "Copies python script to remote host and executes it"
  [host]
  (spit "yum_base.py" (clojure.string/join "\n" yum-script))
  (file-sys/send-file-to host "yum_base.py")
  (let [result (launch "python yum_base.py" :host host)
        output (second (clojure.string/split (:output result) #"\n"))
        yum (ches/parse-string output true)]
    yum))


(defn yum->Distro
  [yum-output]
  (let [[_ major variant] (re-find #"^(\d+)(\w+)" (:releasever yum-output))
        [_ minor] (clojure.string/split (:release yum-output) #"\.")
        keys {:major          major
              :minor          minor
              :release        (:release yum-output)
              :variant        variant
              :distributor-id (-> (clojure.string/replace (:id yum-output) #"Linux" "")
                                  (clojure.string/replace #" " ""))
              :arch           (:basearch yum-output)}]
    (map->Distro keys)))

;; Ughhh, Java doesn't have a good way to get distro information.  So
;; we will scrape it from lsb_release -a
(defn distro-info
  "Runs lsb_release -a and gets the name, version, and variant type:
   {:distributor-id name of the distro (eg Fedora or RHEL)
    :release version number (eg 22 or 7.2)
    :variant type of OS (eg workstation or server)
    :major major release (as integer)
    :minor minor release (as integer)}"
  [host]
  ;; Ughhh this is ugly.  So RHEL 6.x doesn't come with lsb_release.  So we will throw an exception if we're on
  ;; RHEL6, and get the distro info another way
  (try
    (let [info (-> (launch "lsb_release -a" :host host :throws true) :output)
          pattern "^%s:\\s*(.*)$"
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
          variant (second (re-find variant-patt (:distributor-id m)))
          [_ major minor] (re-find #"(\d+)\.?(\d*)" (:release m))
          arch (-> (launch "uname -m" :host host) :output (clojure.string/trim-newline))]
      (map->Distro (merge m {:variant variant :major (Integer/parseInt major)
                             :minor   (Integer/parseInt minor) :arch arch})))
    (catch Exception ex
      ;; We've got RHEL 6, so use yum-base instead
      (yum->Distro (yum-base host)))
    ))


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

;; TODO: use selenium to deletea host
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

(defn system-time-factory
  [host version]
  (letfn [(ntpd? []
            (let [services (launch "systemctl list-unit-files" :host host)]
              (re-find #"ntpd.service\s+enabled" services)))
          (ver7 []
            (let [launch+ #(launch % :host host)]
              (when-not (ntpd?)
                (let [cmds ["yum install -y ntp"
                            "rpm -q ntp"
                            "systemctl stop ntpd.service"
                            "ntpdate clock.redhat.com"
                            "systemctl start ntpd.service"
                            "systemctl enable ntpd.service"]]
                  (list (for [cmd cmds]
                          (launch+ cmd)))))))
          (ver6 []
            (let [launch+ #(launch % :host host)
                  cmds ["service ntpd stop"
                        "ntpdate clock.redhat.com"
                        "systemctl start ntpd.service"
                        "systemctl enable ntpd.service"]]
              (list (for [cmd cmds]
                      (launch+ cmd)))))]
    (match version
           7 ver7
           6 ver6)))

(defmulti disable-firewall
          "Disables the firewall"
          (fn [distro-info host] [(:variant distro-info) (:major distro-info)]))

(defmethod disable-firewall ["Server" 7]
  [_ host]
  (launch "systemctl stop firewalld.service" :host host)
  (launch "systemctl disable firewalld.service" :host host))

(defmethod disable-firewall ["Server" 6]
  [_ host]
  (launch "service iptables stop" :host host)
  (launch "chkconfig iptables off" :host host))

(defn system-setup
  [distro-info host]
  (let [timectl (system-time-factory host (:major distro-info))]
    (timectl)
    (disable-firewall distro-info host)))

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
  (:output (launch (str "rpm -q " pkgname :host host))))

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
    ))

(defn disable-gnome-initial-setup
  "Not sure if this works.  Tries to eliminate the gnome3 splash screen"
  [host]
  (let [distro (distro-info host)
        _ (launch "mkdir -p /etc/skel/.config" :throws false :host host)
        _ (launch "mkdir -p /root/.config" :throws false :host host)
        skel-path "/etc/skel/.config/gnome-initial-setup-done"
        root-path "/root/.config/gnome-initial-setup-done"
        check-n-send #(when-not (file-sys/file-exists? %1 :host host)
                       (spit %1 "yes")
                       (file-sys/send-file-to host "gnome-initial-setup-done" :dest %2))]
    (check-n-send skel-path "/etc/skel/.config")
    (check-n-send root-path "/root/.config")))

(defmulti setup-automation-command-server
          "Sets up ldtp and vnc"
          (fn [dist-info host] [(:major dist-info)]))
