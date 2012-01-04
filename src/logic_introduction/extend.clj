(ns logic-introduction.extend
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(defn ns-fun [nsym fun]
  (fn [a]
    (to-stream 
      (let [all-ns-syms (set (map (comp symbol str) (all-ns)))]
        (->>
          (cond
            ;; ground namespace, can restrict search
            (not (lvar? (walk a nsym))) (for [fun-sym (when (all-ns-syms nsym)
                                                        (-> nsym find-ns ns-publics keys))]
                                          (unify a [nsym fun] [nsym fun-sym]))

            ;; otherwise, search cartesian product
            :else (for [ns (all-ns)
                        fun-sym (-> ns ns-publics keys)]
                    (let [gen-ns (symbol (str ns))]
                      (unify a [nsym fun] [gen-ns fun-sym]))))

          (remove nil?))))))

(comment
(run* [q] (fresh [a b] (== q [a b]) (ns-fun a b)))
(run* [q] (ns-fun 'clojure.core q))
(run* [q] (ns-fun 'clojure.repl q))
(run* [q] (ns-fun 'noexist q))
  )

;  me:  I made a goal that treats the Clojure namespace system as a database
; https://github.com/frenchy64/Logic-Starter/blob/master/src/logic_introduction/extend.clj
; I'd imagine most of the stuff you'd need is in that example
;  Sent at 2:58 AM on Thursday
;  Tassilo:  Hossa, that looks much more complex than I have hoped.
;  me:  not really, i'll explain it
; do you know what a substitution is?
;  Tassilo:  But it's a relation that holds if nsym is the namespace that defines the function fun, right?
; In principle, I know what a substitution is.
;  me:  yes
; more correctly, a substitution list, the terms are interchangable
; core.logic stores lvar->value mappings as a substitution
; and all a goal is, is a function that takes a substitution, and returns a (possibly empty) stream of substituions
; you can see this at the top of the function (fn [a] ;; a is a substituion
; (to-stream ...  ;; returns stream of substituions
; so think of substitutions as "flowing" through each goal
; follow?
;  Tassilo:  Yep
;  me:  here's to-stream https://github.com/clojure/core.logic/blob/master/src/main/clojure/clojure/core/logic.clj#L1433
;  Tassilo:  Ah, ok.  Now I understand.
; That's exactly what I wanted to do on my own.
; Converting lazy seqs to choices.
;  me:  the last important point is the (remove nil?) at the end of ns-fun
;  Tassilo:  What's its purpose?
;  me:  take a step back and read what each clause of the `cond` returns
; `unify` takes a substitution, and two terms to unify. it then returns a new substitution where both terms are equal
; OR returns nil
; ie. cannot unify = nil
; and we wrap these calls in a list comprehension (for)
;  Tassilo:  Ah, ok, I see.
;  me:  so it's really quite elegant and simple
; and you can see the implementation of lvaro in the question of the first clause
; *nonlvaro
;  Sent at 3:11 AM on Thursday
;  Tassilo:  [non]lvaro can only be used inside "real" relations, right?
;  me:  hmm I don't follow
; lvaro is non-relational
; there is another 2 things that will demystify what's happening
; (not (lvar? (walk a nsym)))
; opens up TRS
;  Tassilo:  I mean, using (nonlvaro nsym) won't work, because then the macro expands using a substitution different to a.
; It's a goal on its own and cannot be used in the definition of other goals.
;  me:  you're on the right track
; usually primitive goals like == and conde are not defined in terms of other goals
; then the point is that you use the primitives to compose
; but here, we're making a primitive, basically
;  Tassilo:  yeah, I think I get it
;  me:  great
;  Sent at 3:19 AM on Thursday
;  Tassilo:  I guess, I'll find some time tomorrow to start my implementation.  Thanks to you, it's gonna be a snap. 
;  me:  I had a hunch it was a piece of cake
; I'll dump a bit more exaplanation in this chat window
;  Sent at 3:21 AM on Thursday
;  me:  walk is an implementation function of core.logic. (walk a v) takes a substitution a, and a value v, and returns the value of v in substitution a
; (walk {'a 1} 'a) => 1
; (walk {'a 1} 2) => 2
; you can see, this is how we decide if a lvar is bound
; (walk {'a 1} (lvar 'b)) => _.0
; the last thing to note is that every call to unify uses the same initial substitution we originally passed in at the top of the function
;  Sent at 3:24 AM on Thursday
;  me:  so we essentially rewind every mapping, which is why we return a stream of substitutions. each substitution is a "potential next state" of the original substitution
; and that's it 
; (of course, there is no rewinding. it's all immutable, as usual in clojure)
;  Sent at 3:27 AM on Thursday
;  Tassilo:  Why is (walk {'a 1} 2) => 2?  I mean, there's no value for 2 in the substitution {'a 1}?
;  Sent at 3:28 AM on Thursday
;  me:  conceptually, think of walk as, "in this environment, what does this equal?
; constants always equal themselves
; lvars are looked up in the substitution
; I should be clearer
; (walk {(lvar 'a) 1} 2) => 2
; even that is a bit vague
;  Tassilo:  But conceptually a good explanation.
;  me:  yes
; so ignore the other stuff
; as I am xD
; not important
;  Tassilo:  Already forgotten.  Who are you?!?
; 
;  me:   David wrote the thing remember hehe
;  Sent at 3:33 AM on Thursday
;  me:  walk is covered at the back of the reasoned schemer (TRS)
;  Sent at 3:35 AM on Thursday
;  Tassilo:  I hope to reach the end of TRS soon, but currently I don't have too much time for concentrated reading.
;  Sent at 3:41 AM on Thursday
;  me:  heh, don't be too discouraged if you don't
; brutal book
