(ns uplift.utils.file-sys
  (:require [commando.command :as uc]
            [uplift.utils.algos :refer [varargs]]
            [taoensso.timbre :as timbre]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as cji])
  (:import [java.nio.file Files Paths]
           [java.io File]))


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
  [^String fpath & {:keys [host]}]
  (if host
    (let [cmd (format "ls -al %s" fpath)
          result (uc/ssh  host cmd)]
      (if (= 0 (:exit result)) true false))
    (.exists (File. fpath))))


(defn repo-file-exists?
  [& {:keys [host repo-file]
      :or {repo-file "rhel-latest.repo"}}]
  (let [repo-path (str "/etc/yum.repos.d/" repo-file)]
    (file-exists? repo-path :host host)))


(defn path-name
  [path]
  (.toString (.getFileName path)))


(defn get-remote-file
  "Gets a remote file"
  [host src & {:keys [user dest clean?]
               :or {user "root" dest "." clean? true}}]
  (let [temp "scp %s@%s:%s %s"
        cmd (format temp user host src dest)
        result (uc/launch cmd)
        ]
    result))


(defn send-file-to
  [host src & {:keys [user dest]
               :or {user "root" dest ""}}]
  (let [temp "scp %s %s@%s:%s"
        cmd (format temp src user host dest)]
    (uc/launch cmd)))


(defn leading-slash?
  "Tests if a string has a leading slash"
  [path]
  (= \/ (first path)))


(defn trailing-slash?
  "Tests if a path has a trailing slash"
  [path]
  (= \/ (last path)))


(defn add-trailing-slash
  "Adds a trailing slash to a path"
  [path]
  (str path "/"))


(defn remove-slash
  "Removes trailing and/or leading slashes from a string

  *Args*
  - path: string to remove slashes
  - leading?: if true, removes leading slash (if it exists)
  - trailing?: if true, removes trailing slash"
  [path & {:keys [leading? trailing?]
           :or {leading? true trailing? true}}]
  (letfn [(rm-first [x] (apply str (drop-while (fn [c] (= c \/)) x)))
          (rm-last [x] (apply str (take-while (fn [c] (not= c \/)) x)))
          (rm-both [x] ((comp rm-last rm-first) x))]
    (match [(first path) (last path) leading? trailing?]
           [\/ \/ true true] (rm-both path)
           [\/ \/ true false] (rm-first path)
           [\/ \/ false true] (rm-last path)
           [\/ _ true _] (rm-first path)
           [_ \/ _ true] (rm-last path)
           :else path)))


(defn remove-all-slash
  "Removes all slashes including in the middle"
  [path]
  (->> (clojure.string/split path #"/") (clojure.string/join "")))


(defn path-join
  "Given a sequence of strings, join them together to form a path

  The first element is the parent path, and may include a leading slash.  This indicates that it is an absolute
  path.  If it does not start with a leading slash, then the path is considered relative to the working directory
  If any other element has a preceding or trailing slash it will be removed from the path.
  "
  [& paths]
  (let [parent (first paths)
        parent (if (trailing-slash? parent)
                 (apply str (butlast parent))
                 parent)
        tail (for [p (rest paths)]
               (remove-slash p))
        mkpath (partial cji/file parent)]
    (-> (apply mkpath tail) .getPath)))


(defn str->File
  "Converts a string to a File"
  [path]
  (File. path))


(defn make-path
  [path]
  (Paths/get path (into-array String [])))


(defn path-info
  "Gets commonly used info for a path given as a string"
  [path]
  (let [p (make-path path)
        f (.toFile p)]
    {:file? (.isFile f)
     :dir? (.isDirectory f)
     :exists? (.exists f)
     :parent (.getParent p)
     :filename (.getFileName p)}))


(defn path->file
  ""
  [path]
  (last (clojure.string/split path #"/")))
