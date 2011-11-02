(ns logic-introduction.subs
  (:use [clojure.core.match.core :only [match]])
  (:refer-clojure :exclude [==]))

(def empty-substitution '())

(defn extend-s [x v s]
  (cons (list x v) s))

(def lhs first)
(def rhs second)

(def succeed
  (fn [s] s))

(def fail
  (fn [s] nil))

(deftype LogicVariable [n])

(defn logic-variable [n]
  (LogicVariable. n))

(defn logic-variable? [n]
  (instance? LogicVariable n))

(defmacro fresh [[& names] goal]
  (let [decl (map (fn [n] (list n (list `logic-variable '`n))) names)
        decl (reduce concat decl)]
    `(fn [sub#]
       (let [~@decl]
         (~goal sub#)))))

(defmacro choose-all [& goals]
  `(let [goals# (list ~@goals)]
     (case (count goals#)
       0 succeed
       1 (first goals#)
       (let [g# (first goals#)]
         (fn [s#]
           ((fn [s#]
              ((choose-all (rest goals#)) s#)) ;; TODO monad
             (g# s#)))))))

(defn walk [v s]
  (cond
    (logic-variable? v) (let [as (some #(and (= (lhs %) v) %) s)]
                          (if as
                            (walk (rhs as) s)
                            v))
    :else v))

(defn walk* [v s]
  (let [v (walk v s)]
    (cond
      (logic-variable? v) v
      (or (list? v) 
          (vector? v)) (cons
                         (walk* (first v) s)
                         (walk* (seq (rest v)) s))
      :else v)))

(defn reify-s [v s]
  (let [v (walk v s)]
    (cond
      (logic-variable? v) (extend-s v (str "_" "." (count s)) s)
      (or (list? v)
          (vector? v)) (let [rst (rest v)
                             rst (if (empty? rst)
                                   nil
                                   rst)]
                         (reify-s rst
                                  (reify-s (first v) s)))
      :else s)))

(defn reify [v]
  (walk* v (reify-s v empty-substitution)))

(defn unify [v w s]
  (let [v (walk v s)
        w (walk w s)]
    (cond
      (identical? v w) s
      (logic-variable? v) (extend-s v w s)
      (logic-variable? w) (extend-s w v s)
      (or (and (list? v) (list? w))
          (and (vector? v) (vector? w))) (if-let [s (unify (first v) (first w) s)]
                                           (unify (first v) (first w) s)
                                           nil)
      (= v w) s
      :else nil)))

(defn == [v w]
  (fn [s]
    (if-let [s (unify v w s)]
      (succeed s)
      (fail s))))

(defmacro run [[v] goal]
  `(let [~v (logic-variable '~v)
         s# (~goal empty-substitution)]
     (cond
       (nil? s#) :NO-RESULT
       :else (reify (walk* ~v s#)))))
