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


(defn run
  "Uses a ProcessBuilder to execute a command locally"
  [cmd & opts]
  (let [cmd-s (clojure.string/split cmd #" ")
        run (partial exec/sh cmd-s)
        result @(if opts
                  (run (first opts))
                  (run))]
    (assoc result :cmd (->> cmd-s (interpose " ") (apply str)))))


(defn execute
  [cmd & {:keys [host]}]
  (if host
    (ssh host cmd)
    (run cmd)))


(defn which
  "Determines if a program is in PATH and if so, returns the path if it exists or nil"
  [program & {:keys [host]}]
  (let [result (execute (str "which " program) :host host)]
    (if (= 0 (:exit result))
      (clojure.string/trim (:out result))
      nil)))


(defprotocol Executor
  "Any object that supports execution of a system command should implement this"
  (call [this]))


;; make sure we add ssh-add
(defn ssh-add
  []
  (run "ssh-add"))

;; ==========================================================================================
;; log producer and consumer
;; ==========================================================================================

(defrecord LogProducer
  [;; core.async channel to put lines into
   log-channel
   ;; a function that possibly transforms a line before being put into log-channel
   transformer
   ])


(defrecord LogConsumer
  [log-channel])

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

;; TODO: ughhh, make this a protocol function on Executor
(defn is-alive-process
  [obj]
  (.isAlive obj))

(defn is-alive-ssh
  [obj]
  (= (.getExitStatus obj) -1))

;; TODO: hook the stdout into a network channel
(defn get-output
  [proc & {:keys [logged]}]
  (let [inp (-> (.getInputStream proc) InputStreamReader. BufferedReader.)]
    (loop [line (.readLine inp)
           alive? (is-alive-process proc)]
      (println line)
      (cond line (do
                   (when logged
                     (.append logged (str line "\n")))
                   (recur (.readLine inp) (is-alive-process proc)))
            (not alive?) proc))))


(defn launch
  "Launches a subprocess

  If input, output or error are non-nil, creates a Redirector"
  [cmdr]
  (let [pb (ProcessBuilder. (:cmd cmdr))
        build (comp #(combine-err! % (:combine-err? cmdr))
                    #(set-env! % (:env cmdr))
                    set-dir!)
        _ (build pb (:work-dir cmdr))
        proc (.start pb)]
    (if (:block? cmdr)
      (do
        (get-output proc :logged (:logged! cmdr))
        proc)
      (future (get-output proc :logged (:logged! cmdr))))))


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
    (launch cmdr)))


(defn make-commander
  "Creates a Commander object"
  [cmd & {:keys [work-dir env input output error combine-err? block? logged! close result-handler watch-handler]
          :or   {combine-err?   true
                 block?         true
                 logged!        (StringBuilder.)
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


(defn get-ssh-output
  "Reader for jsch Channel

  *args*
  ssh-res: "
  [ssh-res & {:keys [logged]}]
  (let [chan (:channel ssh-res)
        os (-> (.getInputStream chan) InputStreamReader. BufferedReader.)]
    (if (not (.isConnected chan))
      (.connect chan))
    (println "connected? " (.isConnected chan))
    (loop [status (is-alive-ssh chan)]
      (if status
        (let [line (.readLine os)]
          (println line)
          (when logged
            (.append logged (str line "\n")))
          (recur (is-alive-ssh chan)))
        (do
          (println "Finished with status: " (.getExitStatus chan))
          ssh-res)))))


(defn runner [host cmd]
  (let [ssh-res (ssh host cmd :out :stream)]
    (future (get-ssh-output ssh-res is-alive-ssh))))


(defrecord SSHCommander
  [^String host
   ^String cmd
   ;; TODO: what else do we need?
   ]
  Executor
  (call [this]
    (runner (:host this) (:cmd this))))