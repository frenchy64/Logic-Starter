(ns logic-introduction.numbers
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

;; From "Art of Prolog", Chapter 3

(defn s [a d]
  (conso a [] d))

(def zero  0)
(def one   (first (run 1 [q] (s zero q))))
(def two   (first (run 1 [q] (s one q))))
(def three (first (run 1 [q] (s two q))))
(def four  (first (run 1 [q] (s three q))))
(def five  (first (run 1 [q] (s four q))))
(def six   (first (run 1 [q] (s five q))))

(defn natural-number [x]
  "A relation where x is a natural number"
  (conde
    [(== zero x)]
    [(fresh [p]
       (s p x)
       (natural-number p))]))

(defn <=o [x y]
  "x and y are natural numbers, such that x is less than or
  equal to y"
  (conde
    [(== x zero)
     (natural-number y)]
    [(fresh [xp yp]
       (s xp x)
       (s yp y)
       (<=o xp yp))]))

(defn <o [x y]
  "x and y are natural numbers, such that x is less than y"
  (conde
    [(== x zero)
     (natural-number y)
     (!= x y)]
    [(fresh [xp yp]
       (s xp x)
       (s yp y)
       (<=o xp yp))]))

(defn plus [x y z]
  "x, y, and z are natural numbers such that z is the sum of
  x and y"
  (conde
    [(fresh [a]
       (== [zero a a] [x y z])
       (natural-number a))]
    [(fresh [xp zp]
       (s xp x)
       (s zp z)
       (plus xp y zp))]))

(defn times [x y z]
  "x, y, and z are natural numbers such that z is the product
  of x and y"
  (conde
    [(== [zero zero] [x z])]
    [(fresh [xp xy]
       (s xp x)
       (times xp y xy)
       (plus xy y z))]))

(defn exp [n x y]
  "n, x, and y are natural numbers such that y equals x raised
  to the power n"
  (conde
    [(fresh [np]
       (== [zero zero] [x y])
       (s np n))]
    [(fresh [xp]
       (== [zero one] [n y])
       (s xp x))]
    [(fresh [np z]
       (s np n)
       (exp np x z)
       (times z x y))]))

(defn factorial [n f]
  "f equals n factorial"
  (conde
    [(== [zero one] [n f])]
    [(fresh [np f1]
       (s np n)
       (factorial np f1)
       (times n f1 f))]))

(defn minimum [n1 n2 min]
  "The minimum of the natural numbers n1 and n2 is min"
  (conde
    [(== n1 min)
     (<=o n1 n2)]
    [(== n2 min)
     (<=o n2 n1)]))

(defn modo [x y z]
  "z is the remainder of the integer division of x by y"
  (fresh [q qy]
    (<o z y)
    (times y q qy)
    (plus qy z x)))

(defn ackermann [x y a]
  "a is the value of Ackermann's function for the natural
  numbers x and y"
  (conde
    [(s y a)
     (== zero x)]
    [(fresh [xp]
       (s xp x)
       (== zero y)
       (ackermann xp one a))]
    [(fresh [xp yp val1]
       (s xp x)
       (s yp y)
       (ackermann x yp val1)
       (ackermann xp val1 a))]))

(defn gcd [x y z]
  "z is the greatest common divisor of the natural numbers x and y"
  (conde
    [(modo x y z)
     (gcd y z z)]
    [(== [x zero x] [x y z])
     (<o zero x)]))
