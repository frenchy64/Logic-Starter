(ns logic-introduction.extend
  "Extend core.logic with external databases.
  This example defines a custom goal ns-fun, which acts as a query interface
  to the Clojure namespacing system"
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

;; TODO add note about having to walk any value that's potentially an lvar before
;; using it on a regular Clojure fn

(defn ns-mapo
  [ns-obj k v]
  (fn [a]
    (let [ns-obj (walk a ns-obj)
          k (walk a k)
          v (walk a v)]
      (to-stream
        (->>
          (condp = (map lvar? [ns-obj k v])
            [false false false] (when (= v (ns-resolve ns-obj k))
                                  [a])
            [false false true] (when-let [[_ m] (find (ns-map ns-obj) k)]
                                 [(unify a v m)])
            [false true false] (for [mapping (ns-map ns-obj)]
                                 (when (= v (val mapping))
                                   (unify a k (key mapping))))
            [false true true] (for [current-ns (all-ns)
                                    mapping (ns-map current-ns)]
                                (unify a [k v] mapping))
            [true false false] (for [current-ns (all-ns)
                                     mapping (ns-map current-ns)]
                                 (unify a [ns-obj [k v]] [current-ns mapping]))
            [true false true] (for [current-ns (all-ns)
                                    mapping (ns-map current-ns)]
                                (when-let [[_ m] (find (ns-map ns-obj) k)]
                                  [(unify a [ns-obj v] [current-ns m])]))
            [true true false] (for [current-ns (all-ns)
                                    mapping (ns-map current-ns)]
                                (when (= v (val mapping))
                                  (unify a [ns-obj k] [current-ns (key mapping)])))
            [true true true] (for [current-ns (all-ns)
                                   mapping (ns-map current-ns)]
                               (unify a [ns-obj [k v]] [current-ns mapping])))
          (remove not))))))

(comment
(count (run 10000 [q]
                                 (fresh [n1 n2 a1 a2 v]
                                        (ns-mapo n1 a1 v)
                                        (ns-mapo n2 a2 v)
                                        (!= n1 n2)
                                        (== q [n1 n2 v]))))
  )

(defn ns-fun 
  "nsym and fun are symbols such that nsym names a namespace that contains the function named fun"
  [nsym fun]
  (fn [a]
    (to-stream 
      (let [all-ns-syms (set (map (comp symbol str) (all-ns)))]
        (->>
          (cond
            ;; ground namespace, can restrict search
            (not (lvar? (walk a nsym))) (for [fun-sym (when (all-ns-syms (walk a nsym))
                                                        (-> (walk a nsym) find-ns ns-publics keys))]
                                          (unify a [nsym fun] [nsym fun-sym]))

            ;; otherwise, search cartesian product
            :else (for [ns (all-ns)
                        fun-sym (-> ns ns-publics keys)]
                    (let [gen-ns (symbol (str ns))]
                      (unify a [nsym fun] [gen-ns fun-sym]))))

          (remove not))))))

(defn descendanto
  "Queries the Clojure hierarchy"
  ([parent child] (descendanto nil parent child))
  ([h parent child]
   (fn [a]
     (to-stream
       (->>
         (condp = (map (partial lvar-in? a) [parent child])
           [false false] (when (contains? (apply ancestors (concat (when h [h]) [child])) parent)
                           [a])
           [false true] (for [desc (apply descendants (concat (when h [h]) [parent]))]
                          (unify a [parent child] [parent desc]))
           [true false] (for [ancest (apply ancestors (concat (when h [h]) [child]))]
                          (unify a [parent child] [ancest child]))
           [true true] (throw (Exception. "class-relation cannot have both arguments fresh")))
         (remove not))))))

(comment

(run* [q] (ns-fun 'clojure.core q))
;=> (sorted-map read-line re-pattern keyword? unchecked-inc-int val 
;    chunked-seq? find-protocol-impl vector-of ... )

(run* [q] (ns-fun 'clojure.repl q))
;=> (source stack-element-str set-break-handler! find-doc thread-stopper 
;    demunge apropos dir dir-fn root-cause pst source-fn doc)

(run* [q] (ns-fun 'noexist q))
;=> ()

(run* [q] (ns-fun q 'remove))

(run* [q] (fresh [a b] (== q [a b]) (ns-fun a b)))
;=> ([clojure.set rename-keys] 
;    [clojure.set union] 
;    [clojure.set select] 
;    [clojure.set project] 
;    [clojure.set superset?] 
;    [clojure.set join]
;    ....
;    [clojure.test *report-counters*] 
;    [clojure.test assert-any]
;    ...)
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
;  me:  the last important point is the (remove not) at the end of ns-fun
;  Tassilo:  What's its purpose?
;  me:  take a step back and read what each clause of the `cond` returns
; `unify` takes a substitution, and two terms to unify. it then returns a new substitution where both terms are equal
; OR returns false
; ie. cannot unify = false
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
