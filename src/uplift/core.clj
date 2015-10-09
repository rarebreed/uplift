(ns uplift.core
  (:require [taoensso.timbre :as timbre]
            [clj-commons-exec :as exec]
            [clj-ssh.ssh :as sshs]
            [clj-ssh.cli :as sshc]
            [clojure.java.io :as cjio]
            [uplift.utils.algos :refer [lmap]]
    ;[clojure.tools.nrepl.server :refer [start-server stop-server]]
            clojure.string)
  (:import [java.nio.file Paths Path Files]
           [java.lang.management ManagementFactory]))

;; -----------------------------------------------------------------
(def osb (ManagementFactory/getOperatingSystemMXBean))
(def ddnsname (atom ""))
(def ddnshash (atom ""))
(sshc/default-session-options {:strict-host-key-checking :no})

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


(defn bool
  [i]
  (if (= i 0)
    true
    false))


(defn all? [coll]
  (every? #(when % %) coll))


;; NOTE: Use this function if you're working at the REPL
(defn ssh [host cmd & {:keys [username loglvl]
                       :or {username "root" loglvl :info}}]
  (do
    (timbre/logf loglvl "On %s| Executing command: %s" host cmd)
    (sshc/ssh host cmd :username username)))


;; NOTE:  This function will not work from the REPL.  If you use this, use it from within
;; another clojure program (is there a way to tell you are executing from the repl?)
(defn ssh-p
  "Executes a command on a remote host.
   
  - host: hostname or IP address to execute cmd on
  - cmd: the command to execute"
  [^String host ^String cmd & {:keys [username loglvl pvtkey-path]
                               :or {username "root" loglvl :info pvtkey-path "~/.ssh/id_auto_dsa"}}]
  (timbre/logf loglvl "On %s| Executing command: %s" host cmd)
  (let [agent (sshs/ssh-agent {:use-system-ssh-agent true})
        session (sshs/session agent host {:strict-host-key-checking :no})]
      (sshs/with-connection session
        (sshs/ssh session {:in cmd}))))


(defn run
  "Uses a ProcessBuilder to execute a command locally"
  [cmd & opts]
  (let [cmd-s (clojure.string/split cmd #" ")
        run (partial exec/sh cmd-s)
        result @(if opts
                  (run (first opts))
                  (run))]
    (assoc result :cmd (->> cmd-s (interpose " ") (apply str)))))


(defprotocol Executor
  "Any object that supports execution of a system command should implement this"
  (call [this] [this block?] [this block? throw?]))

;; Wraps the notion of a command
(defrecord Command [cmd host callfn env stdout? stderr? combine? extras])

(defn make-command
  [cmd {:keys [host stdout stderr combine env extras log]
        :as opts
        :or {host nil
             stdout true
             stderr false
             combine true
             env nil
             extras nil
             log false}}]
  (let [func (if host
               ssh
               run)
        obj (map->Command (assoc opts :callfn func))]
    obj))


(defn get-remote-file
  "Gets a remote file"
  [host src & {:keys [user dest]
                :or {user "root" dest "."}}]
  (let [temp "scp %s@%s:%s %s"
        cmd (format temp user host src dest)]
    @(run cmd)))


(defn send-file-to
  [host src & {:keys [user dest]
               :or {user "root" dest ""}}]
  (let [temp "scp %s %s@%s:%s"
        cmd (format temp src user host dest)]
    @(run cmd)))


;; Ughhh, Java doesn't have a good way to get distro information.  So
;; we will scrape it from /etc/os-release
(defn distro-info
  "Scrapes the /etc/*release file to return:
   {:NAME name of the distro (eg Fedora or RHEL)
    :VERSION_ID version number (eg 22 or 7.2
    :VARIANT_ID type of OS (eg workstation or server)}"
  [& relfile]
  (let [rfile (if relfile relfile "/etc/os-release")
        info (:out @(run (str "cat " rfile)))
        pattern "^%s=\\s*(.*)$"
        ;; Create parsers to match NAME, VERSION_ID and VARIANT_ID
        parsers (for [x ["NAME" "VERSION_ID" "VARIANT_ID"]]
                  {(keyword x) (re-pattern (format pattern x))})
        ;; Run each regex on each line, return ([:NAME match?])
        matches (for [parser parsers
                      line (clojure.string/split info #"\n")]
                  (let [keyname (first (keys parser))
                        val (first (vals parser))
                        _ (println "Testing: " line " with " val)]
                    [keyname (re-find val line)]))
        ;; Only get the elements in the seq where the second element isn't nil
        filtered (filter (fn [%] (if (second %) true nil)) matches)
        ;; passed to reduce to return our final map
        finalfn (fn [coll entry]
                  (merge coll (let [f (first entry)
                                    [whole value] (second entry)]
                                (hash-map f value))))]
    (reduce finalfn {} filtered)))


  ;; TODO:  Test this
  (defn git-clone-pull
    [repo]
    (let [f (-> (Paths/get repo (into-array String [])) .toFile)
          localfn #(for [cmd ["git clean -dxf" "git pull"]]
                    (run cmd {:dir %}))
          remotefn #(run (str "git clone " %))]
      (if (.isDirectory f)
        (localfn repo)
        (remotefn repo))))


(defn get-arch
  "Gets the JVM's arch that it is running on.  Note that it returns 
   amd64 (which is the real name, not x86_64) for 64bit on x86"
  []
  (.getArch osb))


(defn ssh-copy-id
  [host & {:keys [user keypath]
           :or {user "root" keypath "/root/.ssh/id_auto_dsa.pub"}}]
  @(run (format "ssh-copy-id -i %s %s@%s" keypath user host)))


;; We will install the openjdk packages including jre and jdk on remote system
(defn install-jdk
  "Will install the Java openjdk on the remote system"
  [host version]
  ;; This assumes we have openjdk in our repos
  (let [jdks (map #(format % version) ["java-1.%d.0-openjdk-devel"
                                       "java-1.%d.0-openjdk"])
        install (map #(ssh host (str "yum install -y " %)) jdks)]
    install))


(defn remote-download
  "ssh'es into remote host and curl the file to dest path on remote host"
  [host url dest]
  (let [cmd (format "curl %s -o %s" url dest)]
    (ssh host cmd)))


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


(defn install-lein
  "Installs leiningen on the remote host and puts it in /usr/local/bin"
  [host dest]
  (let [lein-url "https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein"
        results (try+
                  (remote-download host lein-url dest)
                  (ssh host (format "chmod ug+x %s" dest))
                  (ssh host "lein"))]
    (some #(instance? Exception %) results)))


(defn set-grub-cmdline
  [key val & {:keys [path]
              :or {path ""}}]
  ;; TODO: Get the vecmap for the grub.conf file, and set a new key|val pair
  )


(defn setup-system-time
  "Runs ntpd on system"
  [])


(defn reductor
  "Helps with the common scenario of changing a map based on items in a
   sequence"
  [m entry]
  (let [[k v] entry]
    (assoc m k v)))


(defn varargs [f string+ & strings]
  (f string+ (into-array String (if strings strings []))))


(defn file-seq
  "Returns a sequence of DirectoryStream entries"
  [path]
  (let [p (varargs #(Paths/get %1 %2) path)
        ds (Files/newDirectoryStream p)]
    (for [d ds]
      d)))

(defn list-files
  "Returns a listing of files in a directory"
  [entries & filters]
  (let [filtered (concat
                   (for [f filters]
                     (set (filter f entries))))]
    (for [d filtered]
      (.toString (.getFileName d)))))
