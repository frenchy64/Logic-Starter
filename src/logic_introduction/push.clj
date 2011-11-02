(ns logic-introduction.push
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

;; Calculate palindromes using pushdown automata
;; From "The Art of Prolog" Sterling and Shapiro, 2nd ed, pg 381

(defn palindrome
  ([l] (palindrome l :push []))
  ([l a o]
   (matche [l a o]
           ([[?x . ?xs] :push ?s] (palindrome ?xs :push (lcons ?x ?s)))
           ([[?x . ?xs] :push ?s] (palindrome ?xs :pop (lcons ?x ?s)))
           ([[?x . ?xs] :push ?s] (palindrome ?xs :pop ?s))
           ([[?x . ?xs] :pop [?x . ?s]] (palindrome ?xs :pop ?s))
           ([[ ] :pop [ ]]))))


;  (run* [q]
;        (palindrome [1 2 3 2 1]))
;  => (_.0)
;  (run* [q]
;        (palindrome [1 2 3 2]))
;  => ()
;  (run* [q]
;        (palindrome [1 2 3 2 q]))
;  => (1)
