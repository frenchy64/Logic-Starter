(ns logic-introduction.core
  (:refer-clojure :exclude [inc reify ==])
  (:use [clojure.core.logic minikanren prelude nonrel match disequality]))

(defn geto [k m v]
  (matche [m]
          ([[[k :- v] . _]])
          ([[_ . ?r]] (geto k ?r v))))

(defn typedo [c x t]
  (conde
    ((geto x c t))
    ((matche [c x t]
             ([_ [:apply ?a ?b] _]
              (exist [s]
                     (!= ?a ?b)
                     (typedo c ?b s)
                     (typedo c ?a [s :> t])))))))

