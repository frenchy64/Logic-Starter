(ns logic-introduction.core
  (:refer-clojure :exclude [==])
  (:require (clojure (walk :as w)))
  (:use [clojure.core.logic]))


;; Logic functions

(defn geto [key env value]
  "env is an environment such that the expression key is
  associated with the expression value"
  (matche [env]
          ([[[key :- value] . _]])
          ([[_ . ?rest]] (geto key ?rest value))))

(defn typedo [context exp result-type]
  "context is an environment such that expression exp executed in
  environment context results in type result-type"
  (conde
    ((geto exp context result-type))
    ((matche [context exp result-type]
             ([_ [:apply ?fun ?arg] _]
              (fresh [arg-type]
                     (!= ?fun ?arg)
                     (typedo context ?arg arg-type)
                     (typedo context ?fun [arg-type :> result-type])))))))


;; Frontend functions

(defmacro deftyped [name type arg-vec & body]
  "Defines a function with a strict type"
  (list* `defn
         (with-meta name
                    (assoc (meta name)
                           :type type))
         arg-vec
         body))

(defn- params-to-sym [form]
  (-> (reduce (fn [f x]
                (cons (if (list? x)
                        (params-to-sym x)
                        (with-meta (gensym)
                                   {:type (class x)}))
                      f))
              (list (first form))
              (rest form))
    reverse))

(defn call-to-lambda [form]
  (reduce (fn [f x] 
            (vector 
              :apply
              f
              (if (list? x)
                (call-to-lambda x)
                x)))
          (first form) (next form)))

(defn declared-type [v]
  {:pre [(var? v)]}
  (-> v
    meta
    :type))

(defn extract-environment [form]
  (->> form
    (reduce (fn [f x]
              (cond
                (symbol? x) (apply vector [x :- (or (-> (meta x) :type)
                                                    (declared-type (resolve x))
                                                    (-> x
                                                      eval
                                                      class))]
                                   f)
                (vector? x) (vec (concat f (extract-environment x)))))
            [])
    (filter identity)
    vec))

(defn return-type [form]
  {:pre [(list? form)]}
  (let [f (first form)
        t (declared-type (resolve f))
        get-return (fn get-return [t]
                     (reduce (fn [f x]
                               (if (vector? x)
                                 (get-return x)
                                 x))
                             []
                             t))]
    (get-return t)))

  
(defn type-check-form [form]
  "Type checks a form"
  (let [call (-> form
               params-to-sym
               call-to-lambda)
        env (->> (extract-environment call)
              (apply sorted-set)
              (into []))
        return (return-type form)]
    (run* [q]
          (typedo env
                  call
                  return)
          (== true q))))


;; Example usage

(comment
(deftyped 
  addInteger
  [Integer :> [Integer :> Integer]]
  [x y]
  (+ x y))

(deftyped 
  addDouble
  [Double :> [Double :> Double]]
  [x y]
  (+ x y))

(deftyped
  maxDouble
  [Double :> [Double :> Double]]
  [x y]
  (max x y))

(type-check-form
  '(maxDouble (addDouble 1.0 2.0)
              (addDouble 2.0 3.0)))
;=> (true)

(type-check-form
  '(maxDouble (addDouble 1 2.0)
              (addDouble 2.0 3.0)))
;=> ()

(type-check-form 
  '(addDouble 1.1 2.1))
;=> (true)
)
