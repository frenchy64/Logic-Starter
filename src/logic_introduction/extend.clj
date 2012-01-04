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
