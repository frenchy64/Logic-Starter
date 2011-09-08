(ns logic-introduction.facts
  (:refer-clojure :exclude [inc reify ==])
  (:use [clojure.core.logic minikanren prelude nonrel match disequality tabled]))

;; t2 derives t1
(defrel derives t1 t2)
;; Prefer t1 over t2
(defrel prefer t1 t2)

(fact derives Number Integer)
(fact derives Number Float)
(fact derives Number clojure.lang.BigInt)

(fact prefer Integer Float)
(fact prefer Integer clojure.lang.BigInt)
(fact prefer Float clojure.lang.BigInt)

(fact derives Object Number)
(fact derives Object String)
(fact derives Object Character)

(fact prefer Number String)
(fact prefer Character String)
(fact prefer Number Character)

(def derived-ancestors 
  (tabled [ancestor descendant]
    (conde
      ((derives ancestor descendant))
      ((exist [i]
         (derived-ancestors ancestor i)
         (derives i descendant))))))

;(defn sort-by-preference [ts]
;  (match [ts]
;         ([

(defn ancests [t]
  (run* [q]
        (derived-ancestors q t)))
(defn decents [t]
  (run* [q]
        (derived-ancestors t q)))

