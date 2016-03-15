(ns uplift.utils.reflector
  (:import [org.testng.annotations Test]
           [java.nio.file Paths Files]))

(defn varargs
  "Useful for java interop where a method uses var args"
  [f arg & args]
  (let [t (class arg)]
    (f arg (into-array t (if args args [])))))


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
      (let [name (.toString (.getFileName d))]
        name))))


(defn file-list
  [^String path]
  (let [entries (directory-seq path)]
    (list-files entries)))


(defn no-ext
  "removes .clj extension"
  [f]
  (let [len (count f)]
    (apply str (take (- len 4) f))))


(defn get-annotations
  [path]
  (let [test? (fn [t] (some (meta t) [Test]))
        get-tests (fn [m] (filter test? m))
        get-fns (fn [n] (vals (ns-publics n)))
        files (file-list path)
        namespaces (map #(-> (str "rhsm.gui.tests." (no-ext %)) symbol) files)]
    (apply require namespaces)
    (flatten (map get-tests (map get-fns namespaces)))))


(def test-types #{:acceptance :tier1 :tier2 :tier3})


(defn get-groups
  ([path]
   (for [s (get-annotations path)]
     (let [m ((meta s) Test)]
       {:name (:name (meta s)) :groups (:groups m)})))
  ([]
   (let [p (str (System/getProperty "user.dir") "/src/rhsm/gui/tests")]
     (get-groups p))))


(defn get-tests-by-group
  "Creates a map with 4 keys: acceptance, tier1, tier2, and tier3

  If a defn belongs to one of these groups, it will be stores in a vector"
  [& {:keys [g]
      :or   {g (get-groups)}}]
  (letfn [(updater [curr-val new-val]
            (if (nil? curr-val)
              [new-val]
              (conj curr-val new-val)))
          (gather [acc m]
            (let [{:keys [name groups]} m
                  groups (map keyword groups)
                  group-set (clojure.set/intersection test-types (set groups))]
              (reduce (fn [m k]
                        (update m k updater name)) acc group-set)))]
    (reduce gather {} g)))