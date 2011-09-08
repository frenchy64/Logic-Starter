(ns logic-introduction.facts
  (:refer-clojure :exclude [inc reify ==])
  (:use [clojure.core.logic prelude minikanren match nonrel tabled]))

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

(defne appendo [x y z]
       ([() _ y])
       ([[?a . ?d] _ [?a . ?r]] (appendo ?d y ?r)))

(defn printlno [& rest]
  (== nil (do (apply println rest)
            (flush))))

(defn partition-by-preference [xs pivot littles bigs]
  (matche [xs pivot littles bigs]
          ([[?x . ?xs] ?y [?x . ?littles] ?bigs]
           (conda
             ((== ?x pivot))
             ((derived-ancestor ?x ?y)))
           (partition-by-preference ?xs ?y ?littles ?bigs))
          ([[?x . ?xs] ?y ?littles [?x . ?bigs]]
           (derived-ancestor ?y ?x)
           (partition-by-preference ?xs ?y ?littles ?bigs))
          ([() pivot () ()])))

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


;; logic-introduction.facts=> (run* [q]
;;                                  (sort-by-preference
;;                                    [Number Object] q))
;; ((java.lang.Object java.lang.Number))
;; logic-introduction.facts=> (run* [q]
;;                                  (sort-by-preference
;;                                    [Number Object Integer] q))
;; ((java.lang.Object java.lang.Number java.lang.Integer))
;; logic-introduction.facts=> (run* [q]
;;                                  (sort-by-preference
;;                                    [Integer Number Object ] q))
;; ((java.lang.Object java.lang.Number java.lang.Integer))
;; logic-introduction.facts=> 
