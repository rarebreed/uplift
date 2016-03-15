(ns uplift.utils.reflector
  (:import [org.testng.annotations Test])
  (:require [uplift.utils.file-sys :refer [file-list]]))


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