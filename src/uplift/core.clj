(ns uplift.core
  (:require [taoensso.timbre :as timbre]
            [clj-commons-exec :as exec]
            [clj-ssh.ssh :as sshs]
            [clj-ssh.cli :as sshc]
            [clojure.java.io :as cjio]
    ;[clojure.tools.nrepl.server :refer [start-server stop-server]]
            clojure.string)
  (:import [java.nio.file Paths]
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
        run (partial exec/sh cmd-s)]
    (if opts
      (run (first opts))
      (run))))


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

(def dev "rh72-test-stoner.usersys.redhat.com")

;(install-lein2 dev "/usr/local/bin/lein")


(defn set-grub-cmdline
  [key val & {:keys [path]
              :or {path ""}}]
  ;; TODO: Get the vecmap for the grub.conf file, and set a new key|val pair
     )


(defn setup-system-time
  [])


(defn reductor
  "Helps with the common scenario of changing a map based on items in a
   sequence"
  [m entry]
  (let [[k v] entry]
    (assoc m k v)))