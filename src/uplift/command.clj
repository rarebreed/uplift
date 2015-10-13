(ns uplift.command
  (:require [taoensso.timbre :as timbre]
            [uplift.utils.algos :refer [items]]
            [clj-commons-exec :as exec]
            [clj-ssh.ssh :as sshs]
            [clj-ssh.cli :as sshc]
            [schema.core :as s])
  (:import [java.io OutputStream InputStream]))

(sshc/default-session-options {:strict-host-key-checking :no})

;; NOTE: Use this function if you're working at the REPL
(defn ssh [host cmd & {:keys [username loglvl]
                       :as opts
                       :or {username "root" loglvl :info}}]
  (do
    (timbre/logf loglvl "On %s| Executing command: %s" host cmd)
    (let [opts (merge opts {:username username})
          args (->> (dissoc opts :loglvl) (items) (concat [host cmd]))]
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


(defn run+
  [cmd & {:keys [in out throw? block?]
          :as opts
          :or {throw? false block? true}}]
  (let [cmd-str (clojure.string/split cmd #" ")
        result (if block?
                 @(exec/sh cmd-str)
                 (exec/sh cmd-str))]
    result))


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
  (call [this] [this block?] [this block? throw?]))

;; Wraps the notion of a command
(s/defrecord Command
  [cmd  :- s/Str          ;; command string to execute
   host :- s/Str          ;; hostname or IP
   env                    ;; Map of env vars
   in   :- InputStream    ;; InputStream to stdin
   out  :- OutputStream   ;; OutputStream of stdout
   err  :- OutputStream   ;; OutputStream of stderr
   closed                 ;; Map of file streams to close
   shutdown? :- Boolean   ;; Close process on VM exit
   result-handler-fn      ;; fn that decides success
   ])

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

