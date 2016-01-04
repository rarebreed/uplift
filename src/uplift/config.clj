(ns uplift.config
  (:require [taoensso.timbre :as timbre]
            [clojure.java.io :as cjio]
            [clojure.core.match :refer [match]]
            [uplift.core :refer [reductor]]))

(def ^:dynamic *loglvl* :info)
(defmacro tlog [& body]
  `(timbre/logf ~*loglvl* ~@body))


(defrecord ConfEntry [section line comment key delimiter value])

(defn get-conf-file
  "Converts a config file to a clojure vector of maps

  The vectors elements are a line represented by a map with the following keys:
  :comment if the line is commented out
  :section if the line has [some-section-name] the name of the section
  :delimiter the type of delimiter (eg : or =)
  :key the key in the line
  :value the value for the key
  :line the entire line contents
  "
  [file]
  (let [sect #"^(#)?\s*\[((\w+-?)+)\]"                      ; find section
        patt #"^(#\s*)*\s*(\w+)(\s*[=:]\s*)(.*)"            ; find line, comment, key, delimiter, value
        section (atom "")]
    (with-open [contents (cjio/reader file)]
      (let [lines (line-seq contents)]
        (loop [l lines
               grps []]
          (if (not (empty? l))
            (let [line (first l)
                  section-name (re-find sect line)
                  groups (re-find patt line)
                  section-val (if section-name
                                (do
                                  (timbre/logf :debug "Got a new section")
                                  (reset! section (nth section-name 2)))
                                @section)
                  ;; The map that we collect in the vector
                  base {:section section-val :line line :comment false :key nil :delimiter nil :value nil}
                  entry (if groups
                          (into base (zipmap [:comment :key :delimiter :value] (map #(nth groups %) (range 1 5))))
                          base)]
              (recur (rest l) (conj grps entry)))
            grps))))))


(defn vec-to-file
  "Takes a vector of maps (as returned from get-conf-file) and turns it into a file "
  [vecmap fpath]
  (with-open [newfile (cjio/writer fpath)]
    (doseq [line vecmap]
      (.write newfile (:line line))
      (.newLine newfile))))


(defn indexer [coll]
  (map #(vector % %2) (iterate inc 0) coll))


(defn get-conf-key
  "Takes a vector map (as returned from get-conf-file) a key to search for and a section
   Returns a sequence of 2 element vectors [index line] in which the :line of the map element
   matches key argument

   Example:
     (def confmap (get-conf-file \"/etc/yum.repos.d/fedora.repo\"))
     (def indexed (find-lines confmap \"enabled\" \"fedora-source\"))
     (println indexed)
     [[27 {:section \"fedora-source\", :line \"#enabled=1\", :comment \"#\", :key \"enabled\", :delimiter \"=\", :value \"1\"}]]
     (validate indexed)"
  ([coll key]
    (get-conf-key coll key ""))                               ;; default is no sections
  ([coll key section]
   (let [indexed (indexer coll)
         matcher (fn [[_ line]]
                   (and (= key (:key line))
                        (= section (:section line))))]
     (vec (filter matcher indexed)))))


(defn get-uncommented-entry
  [entries]
  (for [[lineno entry] entries :when (nil? (:comment entry))]
       [lineno entry]))


(defn validate-entries
  "There should be at most 1 entry from find-lines which matches the key and section
   and which is not commented out.  If there is more than one matching key/section
   and is not commented, throw error

   entries is return from find-lines"
  [entries]
  (let [num (count (get-uncommented-entry entries))]
    (if (> num 1)
      (throw (Exception. "Can not have multiple matches per section for the same key"))
      entries)))


(defn mod-vmap
  "'inserts' a new line at index lineno inside the vmap"
  [vmap entry]
  (let [[index item] entry]
    (assoc vmap index item)))


(defn insert-vmap
  "Inserts entry at index.  All originals are shifted
   This works by creating 2 subvecs [0 - index-1], [index - n]
   It then concatenates subvec-left, entry, subvec-right "
  [vmap entry index]
  (let [insert-at (inc index)
        subvec-left (conj (subvec vmap 0 insert-at) entry)
        subvec-right (subvec vmap insert-at)]
    (into subvec-left subvec-right)))


(defn find-last-in-section
  "Allows to find the index of the last entry in a section before a new section begins"
  [vmap section]
  (let [sect-fltr (fn [[_ entry]] (= (:section entry) section))
        indexed (indexer vmap)
        found (filter sect-fltr indexed)]
    (nth (last found) 0)))


(defn set-conf-key
  "Takes a vmap (as returned from get-conf-file) a line number to change, "
  [vmap line-no entry mod]
  (if (= mod :modify)
    (mod-vmap vmap [line-no entry])
    (insert-vmap vmap entry line-no)))


(defn set-conf-file
  "Reads in an existing conf file, and tries to set new value for key

  If the key does not exist, it will write the key-val pair at the end of section.  Note that this function does not
  actually edit the file in place.  It returns a vmap which in turn can be used to write a new config file

  *Args*
  - file: path to conf file
  - key: key to look up
  - value: value to set the key to
  - uncomment?: if key-val is found but is commented out...uncomment the line
  - section: section the key must belong to
  - delimeter: tje separator between key and value
  - newfile: path to where modified file will be written"
  [file key value & {:keys [uncomment? section delimeter]
                     :or   {uncomment? true
                            section    "DEFAULT"
                            delimeter  "="}}]
  (let [vmap (get-conf-file file)                              ;; vector of entries from conf file
        found (validate-entries (get-conf-key vmap key section)) ;; indexed vec of vec of matches on key/section
        ;; FIXME: make it so that if section is nil, it will change all keys in any section
        ;; (ie, do it in a doseq or map)
        uncommented (first (get-uncommented-entry found))
        delim (cond
                uncommented (do
                              (tlog "uncommented")
                              (:delimiter (nth uncommented 1)))
                (not-empty found) (do
                                    (tlog "found was empty")
                                    (:delimiter (nth (first found) 1)))
                :else (do
                        (tlog ":else")
                        delimeter))
        line-no (if uncommented
                  (first uncommented)
                  (let [lastline (find-last-in-section vmap section)]
                    (if lastline
                      lastline
                      (dec (count vmap)))))                       ;; either return last in section index or last line of file
        newline (str key delim value)
        entry {:section section :line newline :comment nil :key key :delimiter delim :value value}
        mod (if uncommented :modify :insert)]
    (set-conf-key vmap line-no entry mod)))


(defn quoter
  "Surrounds one or more lines of text with double quotes"
  [& lines]
  (str "\""  (apply str (interpose " " lines)) "\""))


(defn unquoter
  "Returns a string in which any starting or ending quote is removed"
  [line]
  (let [last-char (last line)
        first-char (first line)]
    (match [first-char last-char]
           [\" \"] (apply str (rest (butlast line)))
           [\" _] (apply str(rest line))
           [_ \"] (apply str (butlast line)))))


(defmulti set-grub-cmdline (fn [x] (:version x)))


(defmethod set-grub-cmdline :rhel7
  [path]
  (let [grub-conf (get-conf-file path)
        modify (fn [[key val]]
                 (let [matches (get-conf-key grub-conf key)   ;; find matches in grub-conf based on key
                       uncommented (first (get-uncommented-entry matches)) ;; find first uncommented match (if any)
                       none-found? (not uncommented)        ;; determines if we modify or insert into grub-conf
                       line-match (if none-found?
                                   [(-> (count grub-conf) dec) {:section "" :comment false :delimiter "="}]
                                   uncommented)
                       [_ entry] line-match
                       newline (if (:line entry)
                                 (str key "=" (quoter (unquoter (:value entry)) val))
                                 (str key "=" (quoter val)))
                       subentry {:line newline :key key :value val}
                       newentry (merge entry subentry)
                       retval [none-found? (assoc line-match 1 newentry)]]
                   retval))
        newentries (vec (map modify [["GRUB_CMDLINE_LINUX" " console=ttyS0"]
                                     ["GRUB_SERIAL_COMMAND" "serial -unit=0 --speed=115200 -word=8 --parity=no --stop=1"]
                                     ["GRUB_CMDLINE_LINUX_DEFAULT" ""]]))
        set-entries (fn [vmap item]
                      (println item)
                      (let [[not-found? [index entry]] item
                            mod (if not-found? :insert :modify)]
                        (set-conf-key vmap index entry mod)))]
    (reduce set-entries grub-conf newentries)))

(defn get-sections
  "Returns a seq of all the sections in a config file"
  [])


(comment
  ;(require '[uplift.core :as uc])
  (require '[clojure.pprint :as cpp])
  ;(def vmap (get-conf-file "/home/stoner/copy.repo"))

  (def f2 (set-conf-file "/home/stoner/copy.repo" "enabled" 1))
  (vec-to-file (set-grub-cmdline "/tmp/grub") "/tmp/newgrub")
  )

