(ns uplift.utils.repl-utils
  (:require [clojure.reflect :as cr]
            [clojure.pprint :as cpp])
  (:import [java.nio.file Paths Path]))

(defn ptable
  [obj & exclusions]
  (let [refl (cr/reflect obj)
        members (for [f (:members refl)] f)
        sorted (sort-by :name members)
        default [:name :parameter-types :return-type :exception-types :flags]
        actual (if exclusions
                 (let [ex (set exclusions)
                       tester (fn [x] (not (ex x)))]
                   (vec (filter tester default)))
                 default)]
    (cpp/print-table actual sorted)))
