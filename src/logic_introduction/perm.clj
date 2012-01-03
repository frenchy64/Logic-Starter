(ns logic-introduction.perm
  (:refer-clojure :exclude [==])
  (:require (clojure (walk :as w)))
  (:use [clojure.core.logic]))

(defrel permission roles ops state)

(defn add-permision [roles ops states]
  (for [r roles
        o ops
        s states]
    (fact permission r o s)))

(add-permision #{:admin :operator} #{:reject :accept} #{:applied})

(defn get-operations [role state]
  (run* [ops]
        (permission role ops state)))

(defn permitted? [role ops state]
  (not 
    (empty?
      (run* [q]
            (permission role ops state)))))
