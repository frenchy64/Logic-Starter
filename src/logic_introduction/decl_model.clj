(ns logic-introduction.decl-model
  (:refer-clojure :exclude [==])
  (:import [java.io Writer])
  (:use ;[clojure.core.logic [minikanren :exclude [LCons walk lfirst lrest lcons?]] prelude nonrel match disequality]
        [clojure.walk :only [walk prewalk postwalk]]
        [clojure.core.match [core :exclude [swap]]]))

(defn- composite?
  "Taken from the old `contrib.core/seqable?`. Since the meaning of 'seqable' is
  questionable, I will work on phasing it out and using a more meaningful
  predicate.  At the moment, the only meaning of `composite?` is:
  Returns true if `(seq x)` will succeed, false otherwise." 
  [x]
  (or (seq? x)
      (instance? clojure.lang.Seqable x)
      (nil? x)
      (instance? Iterable x)
      (-> x .getClass .isArray)
      (string? x)
      (instance? java.util.Map x)))

(defprotocol SetOnce
  (set-once! [this v]))
(defprotocol DFProtocol
  (set-unsafe! [this v]))

(deftype FreshValue [] )

(defn unbound? [d]
  (instance? FreshValue @d))
(defn make-unbound []
  (atom (FreshValue.)))

(deftype LogicVariable [d]
  SetOnce
  (set-once! [_ v]
    (if (unbound? d)
      (do 
        (dosync (swap! d (fn [_] v)))
        true)
      false))

  DFProtocol
  (set-unsafe! [_ v]
    (dosync (swap! d (fn [_] v))))

  clojure.lang.IDeref
  (deref [this] @d))

(defn logic-variable []
  (LogicVariable. (make-unbound)))

(defn reify-solved [v]
  (prewalk (fn [e]
             (if (instance? LogicVariable e)
               (reify-solved @e)
               e))
             v))

(defmulti set-or-equals (fn [l r] [(class l) (class r)]))

(defmethod set-or-equals 
  [LogicVariable LogicVariable]
  [l r]
  (if (identical? l r)
    true
    (match [(unbound? l) (unbound? r)] 
           [true true] (set-once! l r)
           [true false] (set-once! l @r)
           [false true] (set-once! r @l)
           [false false] (set-or-equals @l @r))))

(defn- lvar-w-comp-or-lcons
  [v l]
  (let [a (logic-variable)
        d (logic-variable)]
    (and (set-or-equals v (lcons a d))
         (if (composite? l)
           (and (set-or-equals (first l) a)
                (set-or-equals (rest-nil l) d))
           (and (set-or-equals (lfirst l) a)
                (set-or-equals (lrest l) d))))))

(defn- set-or-equals-df [^LogicVariable df v]
  (or 
    (and (unbound? df)
         (set-once! df v))
    (cond 
      (or (composite? v) (instance? LCons v)) (lvar-w-comp-or-lcons df v)
      :else (= @df v))))

(defmethod set-or-equals 
  [LogicVariable Object]
  [l r]
  (set-or-equals-df l r))

(defmethod set-or-equals 
  [Object LogicVariable]
  [l r]
  (set-or-equals-df r l))

(defmethod set-or-equals 
  [LCons LCons]
  [l r]
  (and (set-or-equals (lfirst l) (lfirst r))
       (set-or-equals (lrest l) (lrest r))))

(defn lcons-with-comp [l r]
  (if-not (or (lcons? r) (composite? r))
    (match [(lcons? r)]
           [true] (and (set-or-equals (lfirst l) (lfirst r))
                       (set-or-equals (lrest l) (lrest r)))
           [false] (and (set-or-equals (lfirst l) (first r))
                        (set-or-equals (lrest l) (rest-nil r)))
           :else false)
    false))

(defmethod set-or-equals 
  [LCons Object]
  [l r]
  (lcons-with-comp l r))
(defmethod set-or-equals 
  [Object LCons]
  [l r]
  (lcons-with-comp r l))



(declare lcons? lfirst lrest)
(defn- rest-nil [n]
  (let [r (rest n)]
    (if (empty? r)
      nil
      r)))


(defmethod set-or-equals 
  [Object Object]
  [l r]
  (or
    (and (composite? l) (composite? r)
         (set-or-equals (first l) (first r))
         (set-or-equals (rest-nil l) (rest-nil r)))
    (= l r)))

(defmethod set-or-equals
  [nil nil]
  [l r]
  true)
(defmethod set-or-equals
  [LogicVariable nil]
  [l r]
  (set-or-equals-df l r))
(defmethod set-or-equals
  [nil LogicVariable]
  [l r]
  (set-or-equals-df r l))
(defmethod set-or-equals
  [Object nil]
  [l r]
  false)
(defmethod set-or-equals
  [nil Object]
  [l r]
  false)

(defmacro choose-all [& goals]
  (let [exp (map (fn [x] (list = true x)) goals)]
    `(and ~@exp)))

(defmacro cond-one 
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

(defmacro let-logic-variable [[& names] & body]
  (let [decl (map (fn [n] (list n (list `logic-variable))) names)
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

(defmacro solve-logic-variable [[n] & body]
  `(let [~n (logic-variable)]
     (if (= true
            (choose-all
              ~@body))
       (reify-solved ~n)
       :logic-introduction.decl-model/NORESULT)))

(defprotocol LConsP
  (lfirst [this])
  (lrest [this]))

(defprotocol LConsPrint
    (toShortString [this]))

(deftype LCons [a d]
  LConsPrint
  (toShortString [this]
                 (cond
                   (.. this getClass (isInstance d)) (str a " " (toShortString d))
                   :else (str a " . " d )))
  Object
  (toString [this] (cond
                     (.. this getClass (isInstance d)) (str "(" a " " (toShortString d) ")")
                     :else (str "(" a " . " d ")")))
  LConsP
  (lfirst [_] a)
  (lrest [_] d))

(defn lcons [a d]
  "Constructs a sequence a with an improper tail d if d is a logic variable."
  (if (or (coll? d) (nil? d))
    (cons a (seq d))
    (LCons. a d )))

(defn lcons? [x]
  (instance? LCons x))

(defmethod print-method LCons [x ^Writer writer]
  (.write writer (str x)))


;; Handy predicates

(defn caro [p a]
  (let [d (logic-variable)]
    (set-or-equals (lcons a d) p)))

(defn cdro [p d]
  (let [a (logic-variable)]
    (set-or-equals (lcons a d) p)))

(defn conso [a d p]
  (set-or-equals (lcons a d) p))


;; Polymorphic type

(defn env-assoc [exp env type]
  "env is an environment such that the expression key is
  associated with the type value"
  (cond-one 
    ((undo-if-false [exp env type]
                    (caro env [exp :- type])))
    ((undo-if-false [exp env type]
                    (let-logic-variable [?rest]
                                        (choose-all
                                          (cdro env ?rest)
                                          (env-assoc exp ?rest type)))))))

(defn expression-check [context exp result-type]
  (cond-one
    ((undo-if-false [exp context result-type]
                    (env-assoc exp context result-type)))
    ((undo-if-false [context exp result-type]
                    (let-logic-variable [fun arg arg-type fun-type]
                                        ;; TODO disequality
                                        (set-or-equals exp [:apply fun arg])
                                        (expression-check context arg arg-type)
                                        (expression-check context fun [arg-type :> result-type]))))))


;; Numbers

(defn s [n]
  "Returns n's succeeding natural number"
  (lcons n []))

(def zero 0)
(def one   (s zero))
(def two   (s one))
(def three (s two))
(def four  (s three))
(def five  (s four))
(def six   (s five))

(defn natural-number [x]
  "A relation where x is a natural number"
  (println "top")
  (let-logic-variable [n]
    (cond-one
      ((undo-if-false [x]
                      (choose-all
                        (do (println x) true)
                        (set-or-equals x zero))))
      ((undo-if-false [x]
                      (choose-all
                        (do (println x) true)
                        (set-or-equals (s n) x)))
       (natural-number n)))))

(defn <=o [x y]
  "x and y are natural numbers, such that x is less than or
  equal to y"
  (let-logic-variable [?x ?y]
    (cond-one
      ((undo-if-false [x y]
                      (set-or-equals x zero))
       (natural-number y))
      ((undo-if-false [x y]
                      (choose-all
                        (set-or-equals (s ?x) x)
                        (set-or-equals (s ?y) y)))
       (<=o ?x ?y)))))

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
;; (let [x (logic-variable)]
;;   (append [1] [2] x)
;;   @x)
;; ;=> [1 2]
;; 
;; Feels like abstracting away assignment


;; # Collecting results
;;
;; Abstract this pattern with "solve-logic-variable". Implementation above.
;;
;; (solve-logic-variable [x]
;;   (append [1] [2] x))
;; ;=> [1 2]
;;
;; Returns the value of logic-variable variable x after executing body.


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
;; Sets the logic-variable variable c# to a appended to b

(defn append-iio [a b c#]
  (match [a b c#]
         [[] _ _] (set-once! c# b)
         [[x & as] _ _] (let [cs# (logic-variable)]
                          (append-iio as b cs#)
                          (set-once! c# (cons x (deref cs#))))))


(defn person [x]
  (cond-one
    ((undo-if-false [x]
       (set-or-equals x 'john)))
    ((undo-if-false [x]
       (set-or-equals x 'andrew)))
    ((undo-if-false [x]
       (set-or-equals x 'james)))))

(defn append-iio [a b c]
  (match [a b c]
         [[] _ _] (set-or-equals c b)
         [[x & as] _ _] (let-logic-variable [cs]
                          (choose-all
                            (append-iio as b cs)
                            (set-or-equals c (cons x (deref cs)))))))

;; # Logical semantics
;;
;; c is a# appended to b
;;
;; # Operational semantics
;;
;; Sets the logic-variable variable a# to the value needed such that
;; a# appended to b equals c

(defn append-oii
  "Declarative model append"
  [a# b c]
  (match [a# b c]
         [_ _ b] (set-once! a# [])
         [_ _ [x & cs]] (let [as# (logic-variable)]
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
;; Instead of "logic-variable" variables we have "logic variables", which are initialized
;; to "fresh".


;(defn appendo [a b c]
;  (matche [a b c]
;          ([[] _ b])
;          ([[?x . ?as] _ [?x . ?cs]]
;           (appendo ?as b ?cs))))

;; # appendo
;;
;; appendo is a relation.
;;
;; Relations are the building blocks of nondeterministic logic programming
;; - no distinguishing between input/output parameters (wow!)
;; - usually built using other relations


;; # run
;;
;; Nondeterministic version of the deterministic solve-logic-variable.
;;
;; Returns a list results. Nondeterministic, multiple results!
;;
;; Specify how many results we wish to collect

;; Emulating the operational semantics of append-iio

;(run 1 [q]
;     (appendo [1] [2] q))
;=> ((1 2))


;; Emulating the operational semantics of append-oii

;(run 1 [q]
;     (appendo q [2] [1 2]))
;=> ((1))


;; Emulating the operational semantics of append-iii

;(run 1 [q]
;     (appendo [1] [2] [1 2]))
;=> (_.0)

;; Unbound logic variables (fresh) are printed like this


;; # Nondeterminism
;;

;(run 4 [q]
;     (exist [a b]
;       (== q [a b])
;       (appendo a b [1 2 3])))
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
