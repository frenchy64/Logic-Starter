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





;; Semantics

;; Two ways to look at a piece of functionality:
;;
;; Logical view: statement of logic
;; Operational view: execution on a computer

;; # Example with Functional Programming
;; ## Logical semantics
;;
;; Return value is a appended to b
;;
;; ## Operational semantics
;;
;; If a is empty, return b, otherwise return rest of a
;; appended to the cons of first of a and b.

(defn append [a b]
  (if (empty? a)
    b
    (let [[x & as] a]
      (append as (cons x b)))))

;; # Eliminate return value
;;
;; Formal logic is normally expressed in terms of variables. This does
;; not map well to the notion of "returning" a value.
;;
;; Convert the return value to a parameter "c" ..
;;
;; (defn append [a b c]
;;    ...)
;;
;; and we can express the Logical Semantics more correctly:
;;
;; c is a appended to b


;; # Directionality
;;
;; In functional programming "return values" are for output
;; and parameters are for "input"
;;
;; To play the role of the return value, we need an "output" parameter.


;; # Output Parameter?
;;
;; The caller of the function should initialize some sort
;; of variable and pass this to the function. After execution
;; we should expect the variable to be changed.
;;
;; Variables should be "bind-once" after being initialized to unbound.
;; 
;; Much safer than "output" parameters with pointers in C, instead bind-once
;; with immutable values. Similar concept.



;; # Dataflow variable
;; 
;; For educational purposes only ..
;;
;; Bind-once semantics.
;;
;; See implementation


;; # Shape
;;
;; Computation has a different shape using output parameters
;;
;; (let [x (dataflow)]
;;   (append [1] [2] x)
;;   @x)
;; ;=> [1 2]
;; 
;; Feels like abstracting away assignment


;; # Collecting results
;;
;; Abstract this pattern with "solve-dataflow". Implementation above.
;;
;; (solve-dataflow [x]
;;   (append [1] [2] x))
;; ;=> [1 2]
;;
;; Returns the value of dataflow variable x after executing body.


;; # Patterns of input/output arguments
;;
;; From the logical semantics of "append", we can deduce 
;; some interesting relationships
;;
;; If c is a appended to b
;; then a should be everything in c before b
;;
;; If c is a appended to b
;; then b should be everything in c after a


;; # append-iio
;;
;; ## Logical semantics
;;
;; c# is a appended to b
;;
;; ## Operational semantics
;;
;; Sets the dataflow variable c# to a appended to b

(defn append-iio [a b c#]
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
;;
;; This requires a paradigm change


;; # Nondeterministic logic programming
;;
;; We've just seen a limited version of deterministic logic programming.
;;
;; Characteristics of deterministic LP
;; - directional (it works for only one pattern of input/output arguments)
;; - deterministic (gives just one solution)
;;
;; Problem: limited mapping from logical semantics to operational semantics.
;;
;; Nondeterministic LP:
;; - more flexible operational semantics


(defn appendo [a b c]
  (matche [a b c]
          ([[] _ _]
           (== b c))
          ([[?x . ?as] _ [?x . ?cs]]
           (appendo ?as b ?cs))))

;; # appendo
;;
;; appendo is a relation.
;;
;; Relations are the building blocks of nondeterministic logic programming
;; - no distinguishing between input/output parameters (wow!)
;; - are themselves built from other relations


;; # run
;;
;; Nondeterministic version of the deterministic solve-dataflow.
;;
;; Returns a list results. Nondeterministic, multiple results!
;;
;; Specify how many results we wish to collect

(run 1 [q]
     (appendo [1] [2] q))
;=> ([1 2])



