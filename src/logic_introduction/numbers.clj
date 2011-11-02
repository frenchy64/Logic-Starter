(ns logic-introduction.numbers
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

;; From "Art of Prolog", Chapter 3

(defn s [n]
  "Returns n's succeeding natural number"
  (llist n []))

(def zero 0)
(def one   (s zero))
(def two   (s one))
(def three (s two))
(def four  (s three))
(def five  (s four))
(def six   (s five))

(defn natural-number [x]
  "A relation where x is a natural number"
  (matche [x]
          ([zero])
          ([(s ?x)] (natural-number ?x))))

(defn <=o [x y]
  "x and y are natural numbers, such that x is less than or
  equal to y"
  (matche [x y]
          ([zero _] (natural-number y))
          ([(s ?x) (s ?y)] (<=o ?x ?y))))

(defn <o [x y]
  "x and y are natural numbers, such that x is less than y"
  (matche [x y]
          ([zero _] (natural-number y) (!= x y))
          ([(s ?x) (s ?y)] (<=o ?x ?y))))

(defn plus [x y z]
  "x, y, and z are natural numbers such that z is the sum of
  x and y"
  (matche [x y z]
          ([zero ?x ?x] (natural-number ?x))
          ([(s ?x) _ (s ?z)] (plus ?x y ?z))))

(defn times [x y z]
  "x, y, and z are natural numbers such that z is the product
  of x and y"
  (matche [x y z]
          ([zero _ zero])
          ([(s ?x) _ _] (fresh [xy]
                               (times ?x y xy)
                               (plus xy y z)))))

(defn exp [n x y]
  "n, x, and y are natural numbers such that y equals x raised
  to the power n"
  (matche [n x y]
          ([(s ?x) zero zero])
          ([zero (s ?x) (s zero)])
          ([(s ?n) _ _] (fresh [z]
                               (exp ?n x z)
                               (times z x y)))))

(defn factorial [n f]
  "f equals n factorial"
  (matche [n f]
          ([zero (s zero)])
          ([(s ?n) _] (fresh [f1]
                             (factorial ?n f1)
                             (times (s ?n) f1 f)))))

(defn minimum [n1 n2 min]
  "The minimum of the natural numbers n1 and n2 is min"
  (matche [n1 n2 min]
          ([_ _ n1] (<=o n1 n2))
          ([_ _ n2] (<=o n2 n1))))

(defn modo [x y z]
  "z is the remainder of the integer division of x by y"
  (<o z y)
  (fresh [q qy]
         (times y q qy)
         (plus qy z x)))

(defn ackermann [x y a]
  "a is the value of Ackermann's function for the natural
  numbers x and y"
  (matche [x y a]
          ([zero ?n (s ?n)])
          ([(s ?m) zero ?val] (ackermann ?m (s zero) ?val))
          ([(s ?m) (s ?n) ?val] (fresh [val1]
                                       (ackermann (s ?m) ?n val1)
                                       (ackermann ?m val1 ?val)))))

(defn gcd [x y z]
  "z is the greatest common divisor of the natural numbers x and y"
  (matche [x y z]
          ([_ _ ?gcd] (modo x y z) (gcd y z ?gcd))
          ([x zero x] (<o zero x))))
