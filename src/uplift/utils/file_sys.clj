(ns uplift.utils.file-sys
  (:require [uplift.command :as uc]
            [uplift.utils.algos :refer [varargs]]
            [taoensso.timbre :as timbre])
  (:import [java.nio.file Files Paths]
           [java.io File])
  )


(defn directory-seq
  "Returns a sequence of DirectoryStream entries"
  [path]
  (let [p (varargs #(Paths/get %1 %2) path)
        ds (Files/newDirectoryStream p)]
    (for [d ds]
      d)))


(defn list-files
  "Returns a listing of files in a directory"
  [entries & filters]
  (let [filters (if (nil? filters)
                  [(fn [_] true)]
                  filters)
        ;; apply the the filter functions to the entries and make them a set
        filtered (reduce clojure.set/union #{}
                         (for [f filters]
                           (set (filter f entries))))]
    (for [d filtered]
      (let [name (.toString (.getFileName d))
            _ (timbre/logf :info name)]
        name))))


(defn file-exists?
  [^String fpath & host]
  (if host
    (let [cmd (format "ls -al %s" fpath)
          result (uc/ssh (first host) cmd)]
      (if (= 0 (:exit result)) true false))
    (.exists (File. fpath))))


(defn repo-file-exists?
  [& {:keys [host repo-file]
      :or {repo-file "rhel-latest.repo"}}]
  (let [repo-path (str "/etc/yum.repos.d" repo-file)]
    (file-exists? repo-path :host host)))


(defn path-name
  [path]
  (.toString (.getFileName path)))


(defn get-remote-file
  "Gets a remote file"
  [host src & {:keys [user dest]
               :or {user "root" dest "."}}]
  (let [temp "scp %s@%s:%s %s"
        cmd (format temp user host src dest)]
    (uc/run cmd)))


(defn send-file-to
  [host src & {:keys [user dest]
               :or {user "root" dest ""}}]
  (let [temp "scp %s %s@%s:%s"
        cmd (format temp src user host dest)]
    (uc/run cmd)))
