(ns logic-introduction.facts
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(defn printlno [& rest]
  (== nil (do (apply println rest)
            (flush))))

;; t2 derives t1
(defrel derives t1 t2)
;; Prefer t1 over t2
(defrel prefer t1 t2)

(fact derives Number Integer)
(fact derives Number Float)

(fact prefer Integer Float)

(fact derives Object Number)
(fact derives Object String)
(fact derives Object Character)

(fact prefer Number String)
(fact prefer Character String)
(fact prefer Number Character)


(def derived-ancestor
  (tabled [ancestor descendant]
    (conde
      ((derives ancestor descendant))
      ((exist [i]
         (derived-ancestor ancestor i)
         (derives i descendant))))))

(defn partition-by-preference [xs pivot littles bigs]
  (matche [xs pivot littles bigs]
          ;; Check for ambiguous preference
          ([[?x . ?xs] ?y _ _]
           (prefer ?x ?y)
           (prefer ?y ?x)
           (project [?x ?y]
                    (throw (Exception. (str "Ambiguous preference: " ?x ?y)))))
          ;; Greater than or equal to pivot ...
          ([[?x . ?xs] ?y [?x . ?littles] _]
           (conde
             ((== ?x ?y))
             ((derived-ancestor ?x ?y))
             ((prefer ?x ?y)))
           (partition-by-preference ?xs ?y ?littles bigs))
          ;; Less than pivot ...
          ([[?x . ?xs] ?y _ [?x . ?bigs]]
           (conde
             ((derived-ancestor ?y ?x))
             ((prefer ?y ?x)))
           (partition-by-preference ?xs ?y littles ?bigs))
          ([() _ () ()])))

(defn sort-by-preference [ts out]
  "Input: ts
  Output: out"
  ;; Quicksort: Pg 70, Art of Prolog
  (matche [ts out]
          ([[?x . ?xs] ?ys]
            (exist [littles bigs ls bs]
                   (partition-by-preference ?xs ?x littles bigs)
                   (sort-by-preference littles ls)
                   (sort-by-preference bigs bs)
                   (appendo ls (lcons ?x bs) ?ys)))
          ([() ()])))


;; logic-introduction.facts=> (fact prefer Float Integer)
;; nil
;; logic-introduction.facts=> (fact prefer Integer Float)
;; nil
;; logic-introduction.facts=> (run* [q]
;;                                  (sort-by-preference
;;                                    [Integer Float Object] q))
;; #<RuntimeException java.lang.RuntimeException: java.lang.Exception: Ambiguous preference: class java.lang.Floatclass java.lang.Integer>
