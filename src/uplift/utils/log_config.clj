;; Creates a simplelogger where stdout is log-level :info,
;; but :debug goes to timestamped file

(ns uplift.utils.log-config
  (:require [taoensso.timbre :as timbre :refer [set-level! set-config!]]
            [clj-time.core :as ct]
            [clj-time.format :as ctf]))

(defn make-timestamp
  [base & {:keys [suffix]
           :or {suffix ".log"}}]
  (let [time-now (ct/now)
        fmt (ctf/formatter "yyyy-MM-dd-HH-mm-ss")]
    (str base "-" (ctf/unparse fmt time-now) suffix)))

(def ^:dynamic *default-log-file* (str "/tmp/" (make-timestamp "commando")))

(defn print-append
  [data]
  (let [{:keys [output-fn]} data]
    (binding [*out* (if (:error? data) *err* *out*)]
      (println (output-fn data)))))

(defn make-file-append
  [fname]
  (fn [data]
    (let [{:keys [output-fn]} data]
      ;; TODO: make sure directory exists
      (spit fname (str (output-fn data) "\n" :append true)))))

(defn make-appender
  [& {:keys [enabled? min-level async? rate-limit output-fn fnc]
      :or {enabled? true
           min-level :debug
           output-fn :inherit}}]
  {:enabled? enabled?
   :min-level min-level
   :async? async?
   :rate-limit rate-limit
   :output-fn output-fn
   :fn fnc
  })

(def print-appender (make-appender :min-level :info :fnc print-append))
(def file-appender (make-appender :fnc (make-file-append *default-log-file*)))

(timbre/set-config! print-appender)
(timbre/merge-config! file-appender)