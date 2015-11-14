(ns uplift.command
  (:require [taoensso.timbre :as timbre]
            [uplift.utils.algos :refer [items]]
            [clj-commons-exec :as exec]
            [clj-ssh.ssh :as sshs]
            [clj-ssh.cli :as sshc]
            [schema.core :as s]
            [clojure.string :refer [split]])
  (:import [java.io BufferedReader InputStreamReader OutputStream InputStream]
           [java.lang ProcessBuilder]
           [java.io File]))

(sshc/default-session-options {:strict-host-key-checking :no})

;; NOTE: Use this function if you're working at the REPL
(defn ssh
  ""
  [host cmd & {:keys [username loglvl]
               :as opts
               :or {username "root" loglvl :info}}]
  (do
    (timbre/logf loglvl "On %s| Executing command: %s" host cmd)
    (let [opts (merge opts {:username username})
          args (->> (dissoc opts :loglvl) (items) (concat [host cmd]))]
      (timbre/info args)
      (apply sshc/ssh args))))


;; NOTE:  This function will not work from the REPL.  If you use this, use it from within
;; another clojure program (is there a way to tell you are executing from the repl?)
;; FIXME: This also doesn't appear to be working
(comment
  (defn ssh-p
    "Executes a command on a remote host.

    - host: hostname or IP address to execute cmd on
    - cmd: the command to execute"
    [^String host ^String cmd & {:keys [username loglvl pvtkey-path]
                                 :or   {username "root" loglvl :info pvtkey-path "~/.ssh/id_auto_dsa"}}]
    (timbre/logf loglvl "On %s| Executing command: %s" host cmd)
    (let [agent (sshs/ssh-agent {:use-system-ssh-agent true})
          session (sshs/session agent host {:strict-host-key-checking :no})]
      (sshs/with-connection session
                            (sshs/ssh session {:in cmd})))))


;; ==========================================================================================
;; The Executor Protocol
;; This represents how to execute a command either locally or remotely.  Remote calls could be
;; done via SSH, uplift.messaging, or even (potentially) a REST call
;; ==========================================================================================
(defprotocol Executor
  "Any object that supports execution of a system command should implement this"
  (call [this] "Execute the process")
  (output [this] "Get saved output of the process"))

;; ==========================================================================================
;; Process
;; Where Executor encapsulates behavior of how to execute a process, Process describes what
;; can be done with the unit of work
;; ==========================================================================================
(defprotocol Worker
  "API for a unit of work"
  (alive? [this] "Is the unit of work still in progress (get it's state)")
  (get-output [this {:keys [logged]}] "Get information from the process in real-time")
  ;(get-error [this] "Get any error information from the process")
  (get-status [this] "The status of a process"))


;; ==========================================================================================
;; LogProducer
;; A LogProducer is a process that emits data of some kind and puts it into a channel.  This
;; can be used by Process types in that they will likely be producing information of interest
;; to other Processes
;; ==========================================================================================
(defrecord LogProducer
  [;; core.async channel to put lines into
   log-channel
   ;; a function that possibly transforms a line before being put into log-channel
   transformer
   ])

;; ==========================================================================================
;; LogConsumer
;; A LogConsumer is a process which takes data out of a channel and does something with it
;; a Process which needs to examine log information or events should probably use this
;; ==========================================================================================
(defrecord LogConsumer
  [log-channel])

;; ==========================================================================================
;;
;; ==========================================================================================
(extend-type java.lang.Process
  Worker

  (alive? [this]
    (.isAlive this))

  (get-output
    [proc {:keys [logged]}]
    (let [inp (-> (.getInputStream proc) InputStreamReader. BufferedReader.)]
      (loop [line (.readLine inp)
             running? (alive? proc)]
        ;; FIXME: abstract the println.  What if user doesn't want to print stdout or wants it to
        ;; to go to a network channel or to a core.async channel?
        (println line)
        (cond line (do
                     (when logged
                       (.append logged (str line "\n")))
                     (recur (.readLine inp) (alive? proc)))
              (not running?) proc
              :else (do
                      (timbre/warn "unknown condition in get-output")
                      proc)))))
  (get-status [this]

    (.exitValue this)))


;; ==========================================================================================
;; reimplementation of the teleproc project in clojure
;; ==========================================================================================

(defn set-dir!
  [pb dir]
  {:pre [#(if dir (.exists dir) true)]}
  (when dir
    (.directory pb dir))
  pb)

(defn set-env!
  [pb env]
  (when env
    (.environment pb env))
  pb)

(defn combine-err!
  [pb combine?]
  (.redirectErrorStream pb combine?)
  pb)


;; ==========================================================================================
;; Commander
;; Represents how to call a local subprocess
;; ==========================================================================================
(defrecord Commander
  [cmd                                                      ;; vector of String
   ^File work-dir                                           ;; working directory
   env                                                      ;; environment map to be used by process
   ^OutputStream input                                      ;; A stream to send data to stdin
   ^InputStream output                                      ;; A stream to contain stdout
   ^InputStream error                                       ;; An OutputStream connected to stderr
   ^Boolean combine-err?                                    ;; redirect stderr to stdout?
   ^Boolean block?
   logged!                                                  ;; holds output/err
   close                                                    ;; map of which streams to close :in, :out :err
   result-handler                                           ;; fn to determine success
   watch-handler                                            ;; function launched in a separate thread
   ]
  Executor

  (call [cmdr]
    (let [pb (ProcessBuilder. (:cmd cmdr))
          build (comp #(combine-err! % (:combine-err? cmdr))
                      #(set-env! % (:env cmdr))
                      set-dir!)
          _ (build pb (:work-dir cmdr))
          logger (:logged! cmdr)
          proc (.start pb)]
      (if (:block? cmdr)
        (do
          (get-output proc {:logged logger})
          proc)
        (future (get-output proc {:logged logger})))))

  (output [cmdr]
    (.toString (:logged! cmdr))))


(defn make->commander
  "Creates a Commander object"
  [cmd & {:keys [work-dir env input output error combine-err? block? logged! close result-handler watch-handler]
          :or   {combine-err?   true
                 block?         true
                 logged!        (StringBuilder. 1024)
                 close          {:in false :out false :err false}
                 result-handler (fn [res]
                                  (= 0 (:out res)))}
          :as   opts}]
  (map->Commander (merge opts {:cmd (if (= String (class cmd))
                                      (split cmd #"\s+")
                                      cmd)
                               :work-dir (when work-dir
                                           (File. work-dir))
                               :combine-err? combine-err?
                               :block? block?
                               :logged! logged!
                               :close close
                               :result-handler result-handler})))


(defrecord SSHProcess
  [channel
   out-stream
   err-stream
   session]
  Worker

  (alive? [this]
    (let [chan (:channel this)]
      (= (.getExitStatus chan) -1)))

  (get-output [this {:keys [logged]}]
    (let [chan (:channel this)
          os (-> (.getInputStream chan) InputStreamReader. BufferedReader.)]
      (if (not (.isConnected chan))
        (.connect chan))
      (println "connected? " (.isConnected chan))
      (loop [status (alive? this)]
        (if status
          ;; While the channel is still open, read the stdout that was piped to the InputStream
          (let [line (.readLine os)]
            (println line)
            (when logged
              (.append logged (str line "\n")))
            (recur (alive? this)))
          ;; There might be info in the BufferedReader once the channel closes, so read it
          (do
            (while (.ready os)
              (let [line (.readLine os)]
                (println line)
                (.append logged (str line "\n"))))
            (println "Finished with status: " (.getExitStatus chan))
            this)))))

  (get-status [this]
    (let [chan (:channel this)]
      (.getExitStatus chan))))


(defn make->SSHProcess
  [ssh-res]
  (map->SSHProcess ssh-res))


(defrecord SSHCommander
  [^String host
   ^String cmd
   ^StringBuilder logged!
   result-handler
   watch-handler
   ;; TODO: what else do we need?
   ]
  Executor
  (call [this]
    (let [host (:host this)
          cmd (:cmd this)
          logger (:logged! this)
          ssh-res (make->SSHProcess (ssh host cmd :out :stream))]
      (future (get-output ssh-res {:logged logger}))))
  (output [this]
    (.toString (:logged! this))))


(defn make->sshcommander
  [host cmd & {:keys [logger result-handler watch-handler]
               :or {logger (StringBuilder. 1024)}
               :as opts}]
  (let [m {:host host :cmd cmd :logger logger :result-handler result-handler :watch-handler watch-handler}]
    (println m)
    (map->SSHCommander m)))


(defn reducer [m]
  "flattens a map (one-level) by turning it into a sequence of (k1 v1 k2 v2 ..)"
  (reduce #(concat %1 %2) []
          (for [[k v] m]
            [k v])))

(defn launch
  "Improved way to launch a command"
  [cmd & {:keys [host]
          :as opts}]
  (let [command (if host
                  (make->sshcommander host cmd)
                  (apply make->commander cmd (reducer opts)))]
    [command (call command)]))


;; TODO: use the Executor protocol
(defn ^{:deprecated true} which
  "Determines if a program is in PATH and if so, returns the path if it exists or nil"
  [program & {:keys [host]}]
  (let [[cmd proc] (launch (str "which " program) :host host)
        proc (if (future? proc) @proc proc)]
    (if (= 0 (get-status proc))
      (clojure.string/trim (output cmd))
      nil)))

;(launch+ "ssh-add")
