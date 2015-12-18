(ns uplift.utils.repl-utils
  (:require [clojure.reflect :as cr]
            [clojure.pprint :as cpp]
            [clojure.repl :refer [source-fn]]))

(comment
  (do
    (require '[clojure.reflect :as cr]
             '[clojure.pprint :as cpp]))
  )

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


(defn functions [ns]
  (filter #(fn? (deref %)) (vals (ns-publics ns))))

(defn code [fn-seq]
  (for [fnc fn-seq]
    (-> (meta fnc) :name source-fn read-string)))

(defmacro get-code-
  []
  `(let [fns# ~(functions *ns*)]
     (for [fnc# fns#]
       (-> (meta fnc#) :name source-fn read-string))))


(defn get-code []
  (let [fns (functions *ns*)]
    (for [fnc fns]
      (-> (meta fnc) :name source-fn read-string))))


;; I think there's some hairy corners here.  For starters, I dont understand why I
;; needed to do an eval.  Without it, the macro should return something like:
;; (defn foo
;;   [x]
;;   (newcode ...)
;;   (oldcode))
;; Since the macro returns that, shouldn't it be implicitly eval'ed?
;; Also, this macro will (I believe) always intern the new function in whatever
;; namespace it was invoked from, which may not be the desired behavior
(defmacro mod-fn [fn-list n newcode]
  "Takes a function-as-list, splits it at n, and inserts newcode at the split point

  fn-list is now a new defn which gets eval'ed and interned into the namespace"
  `(let [code# ~fn-list
         [f# s#] (split-at ~n code#)
         s-# (conj s# ~newcode)
         final# (concat f# s-#)]
     (eval (apply list final#))))


;; Contains most of the information as the metadata with 2 additions.  It contains the
;; index of where the body of the function begins, and also where it ends.  This allows
;; a user to more conveniently insert new code at the beginning or end of a function
(defrecord FunctionInfo
  [metadata
   name
   docstring
   args
   testmap
   body
   body-start
   body-end])


;; ughhhh way too imperative.  there must be a more functional way to do this.
;; loop/recur maybe?
(defn make->FunctionInfo
  "Given a function as a list, return the metadata, name, docstring, args, and
  index where the body starts and ends

  We can't just use the metadata for this, because we need to know at what position
  in the list the body of the function starts at"
  [fun]
  (let [pop+ #(let [[h & t] %]
               [h t])
        index (atom 2)
        [_ name & rem] fun
        [doc rem] (if (= String (type (first rem)))
                    (do
                      (swap! index inc)
                      (pop+ rem))
                    ["" rem])
        [args rem] (do
                     (swap! index inc)
                     (pop+ rem))
        [testmap rem] (if (and (= clojure.lang.PersistentHashMap (type (first rem)))
                               (or (contains? (first rem) :pre)
                                   (contains? (first rem) :post)))
                        (do
                          (swap! index inc)
                          (pop+ rem))
                        [nil rem])
        fi {:metadata   (meta (second fun))
            :name       name
            :docstring  doc
            :args       args
            :testmap    testmap
            :body       rem
            :body-start @index
            :body-end   (count fun)}]
    (map->FunctionInfo fi)))
