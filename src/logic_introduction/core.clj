(ns logic-introduction.core
    (:refer-clojure :exclude [inc reify ==])
    (:use [clojure.core.logic minikanren prelude nonrel match disequality]))

(defn geto [key env value]
  "Succeed if type association [key :- value] is found in vector env."
  (matche [env]
          ([[[key :- value] . _]])
          ([[_ . ?rest]] (geto key ?rest value))))

(defn typedo [context exp result-type]
  "Succeed if exp executed in context results in a result-type"
  (conde
    ((geto exp context result-type))
    ((matche [context exp result-type]
             ([_ [:apply ?fun ?arg] _]
              (exist [arg-type]
                     (!= ?fun ?arg)
                     (typedo context ?arg arg-type)
                     (typedo context ?fun [arg-type :> result-type])))))))

