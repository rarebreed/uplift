(ns uplift.utils.algos
  (:require [clojure.core.match :refer [match]]))

(defn lmap
  "Sometimes you don't want to iterate through every item in a sequence
  This function will walk through the sequence applying f to each item
  and then applying checkfn to the result of f.  If/when checkfn returns
  a falsey value iteration will stop"
  [f checkfn seq]
  (lazy-seq
    (let [h (first seq)
          t (rest seq)
          result (f h)
          continue (checkfn result)]
      (if (and continue (not-empty t))
        (cons result (lmap f checkfn t))
        nil))))


(defn items
  [m]
  (flatten (for [[k v] m] [k v])))


;; Useful for java interop where a java function is a vararg function
(defn varargs [f arg & args]
  (let [t (class arg)]
    (f arg (into-array t (if args args [])))))


(defmulti bool
          "Truthiness multimethod.  As more types need to be truthified, add them to the cond"
          (fn [x]
                 (cond (= (type x) String) :string
                       :else :number)))

(defmethod bool :number
  [i]
  (if (= i 0) true false))


(defmethod bool :string
  [i]
  (if (= i "0") true false))


(defn all? [coll]
  (every? #(when % %) coll))


(defn rand-coll
  "Get random elements from a collection"
  [coll size]
  (take size (shuffle coll)))


(defn comp<
  "A reverse comp that works from left to right"
  [& fns]
  (apply comp (reverse fns)))

(def keywordize (comp
                  keyword
                  #(clojure.string/replace % #"\s+" "-")
                  clojure.string/lower-case))

(defn xor
  "Exclusive OR"
  [x y]
  (match [x y]
         [true true] false
         [false false] false
         :else true))
