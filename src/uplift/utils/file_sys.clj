(ns uplift.utils.file-sys
  (:require [commando.command :as uc]
            [uplift.utils.algos :refer [varargs]]
            [taoensso.timbre :as timbre]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as cji]
            [uplift.utils.log-config :as lc :refer [make-timestamped-file]])
  (:import [java.nio.file Files Paths]
           [java.io File]))

(defn make-path
  [path]
  (Paths/get path (into-array String [])))


(def str->Path
  "Alias for make-path"
  make-path)


(defn delete-file
  ([path]
   (let [p (str->Path path)]
     (try
       (Files/delete p)
       (catch Exception ex (format "Could not delete %s" path)))))
  ([host path]
    (uc/launch (format "rm -f %s" path) :host host) :throws? true))


(defn path-info
  "Gets commonly used info for a path given as a string. "
  [path]
  (let [p (make-path path)
        f (.toFile p)]
    {:file? (.isFile f)
     :dir? (.isDirectory f)
     :exists? (.exists f)
     :parent (.getParent p)
     :filename (.getFileName p)}))

(defn directory-seq
  "Returns a sequence of DirectoryStream entries"
  [^String path]
  (let [p (varargs #(Paths/get %1 %2) path)
        ds (Files/newDirectoryStream p)]
    (for [d ds]
      d)))


(defn list-files
  "Returns a listing of files in a directory

  Only works locally"
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

(defn file-list
  [^String path]
  (let [entries (directory-seq path)]
    (list-files entries)))

(defn path-name
  [path]
  (.toString (.getFileName path)))

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


(defn path->file
  ""
  [path]
  (last (clojure.string/split path #"/")))


(defn file-exists?
  [^String fpath & {:keys [host]}]
  (if host
    (let [cmd (format "ls -al %s" fpath)
          result (uc/launch cmd :host host)]
      (if (= 0 (:status result)) true false))
    (.exists (File. fpath))))


(defn repo-file-exists?
  [& {:keys [host repo-file]
      :or {repo-file "rhel-latest.repo"}}]
  (let [repo-path (str "/etc/yum.repos.d/" repo-file)]
    (file-exists? repo-path :host host)))


(defn get-remote-file
  "Gets a remote file"
  [host src & {:keys [user dest clean?]
               :or {user "root" dest "." clean? true}}]
  (let [temp "scp %s@%s:%s %s"
        cmd (format temp user host src dest)
        _ (uc/launch cmd :throws? true)
        info (path-info src)]
    (path-join dest (.toString (:filename info)))))


(defn send-file-to
  [host src & {:keys [user dest]
               :or {user "root" dest ""}}]
  (let [temp "scp %s %s@%s:%s"
        cmd (format temp src user host dest)]
    (uc/launch cmd :throws? true)))


(defn make-backup
  "Checks to see if a file exists named by orig.

  If it finds an alternative file ending, it will generate a timestamped new ending"
  [fpath & {:keys [host]}]
  (let [{:keys [filename parent]} (path-info fpath)
        [orig dir] (for [d [filename parent]]
                     (.toString d))
        orig-patt (re-pattern (format "^%s(\\..*)*" orig))
        entries (-> (uc/launch (str "ls -A " dir) :host host) :output (clojure.string/split #"\n"))
        m (for [e entries]
            (re-matches orig-patt e))
        matched (-> (filter #(not (nil? %)) m) sort)
        fullpath (path-join dir orig)
        _ (when (empty? matched)
            (throw (Exception. (format "%s does not exist" fullpath))))
        ts (lc/make-timestamp)
        backup (str (first (first matched)) "." ts)
        backup-path (path-join dir backup)]
    ;; copy /dir/orig to /dir/ts
    {:all m :matches matched :ts backup :fullpath fullpath :backup backup-path
     :result (uc/launch (format "cp %s %s" fullpath backup-path) :host host)}))