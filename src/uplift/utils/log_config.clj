;; Creates a simplelogger where stdout is log-level :info,
;; but :debug goes to timestamped file

(ns uplift.utils.log-config
  (:require [taoensso.timbre :as timbre :refer [set-level! set-config!]]
            [clj-time.core :as ct]
            [clj-time.format :as ctf]))

(defn make-timestamp
  [& {:keys [fmt]
      :or {fmt "yyyy-MM-dd-HH-mm-ss"}}]
  (let [time-now (ct/now)
        fmt (ctf/formatter fmt)]
    (ctf/unparse fmt time-now)))

(defn make-timestamped-file
  [base & {:keys [suffix]
           :or {suffix ".log"}}]
  (let [ts (make-timestamp)]
    (str base "-" ts suffix)))

(defn timbre-mw
  "Reduces the namespace output"
  [data-map]
  (let [ns-info (:?ns-str data-map)
        parts (clojure.string/split ns-info #"\.")
        ns-partial (clojure.string/join "." (map #(first %) (butlast parts)))
        ns-to-use (str ns-partial "." (last parts))]
    (assoc data-map :?ns-str ns-to-use)))


(defn timbre-mw-nohostname
  [data-map]
  (assoc data-map :hostname_ ""))


(def ^:dynamic *default-log-file* (str "/tmp/" (make-timestamped-file "commando")))

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

(defn merge-timbre-mw
  "Installs middlewares (mw) into timbre's config"
  [mw]
  (timbre/merge-config!
    {:middleware mw}))

(merge-timbre-mw [timbre-mw timbre-mw-nohostname])