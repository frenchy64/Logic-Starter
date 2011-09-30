(ns logic-introduction.decl-model
  (:refer-clojure :exclude [inc reify == compile parse])
  (:use [clojure.core.logic minikanren prelude nonrel match disequality]
        [clojure.core.match [core :exclude [swap]]]))

(defprotocol SetOnce
  (set-once! [this v]))
(defprotocol DFProtocol
  (set-unsafe! [this v]))

(def ^:private unbound-dataflow :logic-introduction.decl-model/UNBOUND-DATAFLOW)

(defn unbound? [d]
  (= @d unbound-dataflow))
(defn make-unbound []
  (atom unbound-dataflow))

(deftype DataFlow [d]
  SetOnce
  (set-once! [_ v]
    (if (unbound? d)
      (dosync (swap! d (fn [_] v)))
      (throw (Exception. "Already bound"))))

  DFProtocol
  (set-unsafe! [_ v]
    (dosync (swap! d (fn [_] v))))


  clojure.lang.IDeref
  (deref [this] @d))

(defn dataflow []
  (DataFlow. (make-unbound)))


(defmulti set-or-equals (fn [l r] [(class l) (class r)]))

(defmethod set-or-equals 
  [DataFlow DataFlow]
  [l r]
  (throw (Exception. "not ready"))
  (match [(unbound? l) (unbound? r)] 
         [true true] false ;;TODO
         [true false] false ;(share-dataflow r l)
         [false true] false
         [false false] false))

(defn- set-or-equals-df [^DataFlow df v]
  (match [(unbound? df)]
         [true] (do 
                  (set-once! df v)
                  true)
         [false] (= @df v)))

(defmethod set-or-equals 
  [DataFlow Object]
  [l r]
  (set-or-equals-df l r))

(defmethod set-or-equals 
  [Object DataFlow]
  [l r]
  (set-or-equals-df r l))

(defmethod set-or-equals 
  [Object Object]
  [l r]
  (= r l))

(defmacro choose-all [& goals]
  (let [exp (map (fn [x] (list = true x)) goals)]
    `(and ~@exp)))

(defmacro choose-one 
  "Clauses are lists of goals, each with one question
  and 0+ answers"
  [& clauses]
  (let [cl (map (fn [[q & a]] (list
                                (list = true q) 
                                (list* `choose-all a))) clauses)
        cl (reduce concat cl)]
    `(cond
       ~@cl
       :else false)))

(defmacro let-dataflow [[& names] & body]
  (let [decl (map (fn [n] (list n (list `dataflow))) names)
        decl (reduce concat decl)]
    `(let [~@decl]
       true
       ~@body)))

(defmacro undo-if-false [[& dfvars] & body]
  `(let [~'old-vals (map deref (list ~@dfvars))]
     (if (= false (choose-all ~@body))
       (do (map #(set-or-equals %1 %2) (list ~@dfvars) ~'old-vals)
           false)
       true)))

(defmacro solve-dataflow [[n] & body]
  `(let [~n (dataflow)]
     (if (= true
            (choose-all
              ~@body))
       (deref ~n)
       :logic-introduction.decl-model/NORESULT)))



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
  (match [a b]
         [[] _] b
         [[x & as] _] (cons x (append as b))))

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
  (match [a b c#]
         [[] _ _] (set-once! c# b)
         [[x & as] _ _] (let [cs# (dataflow)]
                          (append-iio as b cs#)
                          (set-once! c# (cons x (deref cs#))))))


(defn person [x]
  (choose-one
    ((undo-if-false [x]
       (set-or-equals x 'john)))
    ((undo-if-false [x]
       (set-or-equals x 'andrew)))
    ((undo-if-false [x]
       (set-or-equals x 'james)))))

(defn append-iio [a b c]
  (match [a b c]
         [[] _ _] (set-or-equals c b)
         [[x & as] _ _] (let-dataflow [cs]
                          (choose-all
                            (append-iio as b cs)
                            (set-or-equals c (cons x (deref cs)))))))

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
  (match [a# b c]
         [_ _ b] (set-once! a# [])
         [_ _ [x & cs]] (let [as# (dataflow)]
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
;;
;; This is core.logic, an implementation of minikanren.
;;
;; Instead of "dataflow" variables we have "logic variables", which are initialized
;; to "fresh".


(defn appendo [a b c]
  (matche [a b c]
          ([[] _ b])
          ([[?x . ?as] _ [?x . ?cs]]
           (appendo ?as b ?cs))))

;; # appendo
;;
;; appendo is a relation.
;;
;; Relations are the building blocks of nondeterministic logic programming
;; - no distinguishing between input/output parameters (wow!)
;; - usually built using other relations


;; # run
;;
;; Nondeterministic version of the deterministic solve-dataflow.
;;
;; Returns a list results. Nondeterministic, multiple results!
;;
;; Specify how many results we wish to collect

;; Emulating the operational semantics of append-iio

(run 1 [q]
     (appendo [1] [2] q))
;=> ((1 2))


;; Emulating the operational semantics of append-oii

(run 1 [q]
     (appendo q [2] [1 2]))
;=> ((1))


;; Emulating the operational semantics of append-iii

(run 1 [q]
     (appendo [1] [2] [1 2]))
;=> (_.0)

;; Unbound logic variables (fresh) are printed like this


;; # Nondeterminism
;;

(run 4 [q]
     (exist [a b]
       (== q [a b])
       (appendo a b [1 2 3])))
;=> ([[] [1 2 3]] 
;    [(1) (2 3)] 
;    [(1 2) (3)] 
;    [(1 2 3) ()])


;; # exist
;;
;; introduces lexically scoped logic variables
;;


;; # ==
;;
;; Unifies its arguments
