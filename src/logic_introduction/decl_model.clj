(ns logic-introduction.decl-model
  (:refer-clojure :exclude [inc reify == compile parse])
  (:use [clojure.core.logic minikanren prelude nonrel match disequality]
        [clojure.core.match [core :exclude [swap]]]))

(defmacro solve-dataflow [[n] & body]
  `(let [~n (dataflow)]
     ~@body
     (deref ~n)))

(defprotocol SetOnce
  (set-once! [this v]))

(def ^:private unbound-dataflow (Object.))

(deftype DataFlow [value]
  SetOnce
  (set-once! [_ v]
    (if (= @value unbound-dataflow)
      (dosync (swap! value (fn [_] v)))
      (throw (Exception. "Already bound"))))
  clojure.lang.IDeref
  (deref [this] @value))

(defn dataflow []
  (DataFlow. (atom unbound-dataflow)))

;; # Logical semantics
;;
;; c# is a appended to b
;;
;; # Operational semantics
;;
;; Sets the dataflow variable c# to a appended to b

(defn append-iio
  "Declarative model append"
  [a b c#]
  (if (empty? a)
    (set-once! c# b)
    (let [[x & as] a
          cs# (dataflow)]
      (append-iio as b cs#)
      (set-once! c# (cons x (deref cs#))))))


;; # Logical semantics
;;
;; c is a# appended to b
;;
;; # Operational semantics
;;
;; Sets the dataflow variable a# to the value needed such that
;; a# appended to b equals c

(defn append-oii
  "Declarative model append"
  [a# b c]
  (if (= b c)
    (set-once! a# [])
    (let [[x & cs] c
          as# (dataflow)]
      (append-oii as# b cs)
      (set-once! a# (cons x (deref as#))))))

;; What about append-ooi ?
;;
;;  (append-ooi x# y# [1 2 3])
;;
;; There are 4 different solutions that satisfy the
;; logical semantics.
;;
;; x = [], y = [1 2 3]
;; x = [1], y = [2 3]
;; x = [1 2], y = [3]
;; x = [1 2 3], y = []
;;
;; Our model is deterministic (it gives just one solution).


;; Have you noticed that the logical semantics are identical for
;; all versions of append, yet the operational semantics are different?
;;
;; What if we could model the operational semantics in terms of the logical
;; semantics?

(defn appendo [x y z]
  (matche []
