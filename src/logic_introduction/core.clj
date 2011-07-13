(ns logic-introduction.core
  (:refer-clojure :exclude [inc reify ==])
  (:use [clojure.core.logic minikanren prelude nonrel match]))

(defn findo [x l o]
  (matcha [l]
          ([[[?y :- o] . _]] 
           (project [x ?y] (== (= x ?y) true)))
          ([[_ . ?c]] (findo x ?c o))))

(defn typedo [c x t]
  (conda
    ((lvaro x) (findo x c t))
    ((matche [x]
             ([[:apply ?a ?b]]
              (exist [s]
                     (typedo c ?a [s :> t])
                     (typedo c ?b s)))))))
