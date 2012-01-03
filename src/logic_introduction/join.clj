(ns logic-introduction.join
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

;http://stackoverflow.com/questions/8705001/how-to-simulate-an-outer-join-in-core-logic

(defrel items Name Color)
(defrel restricted-to Country Name)
(defrel not-allowed-in Country Name)

(facts items [['Purse 'Blue]
              ['Car 'Red]
              ['Banana 'Yellow]])

(facts restricted-to [['US 'Car]])

(facts not-allowed-in [['UK 'Banana]
                       ['France 'Purse]])


;; The item must be in the list of items
;; The country/item must be not be in the 'not-allowed-in' list
;; Either:
;; There is no country in the restricted-to list for that item
;; The country/item pair is in the restricted-to list

(defn get-items-colors-for-country [country]
  (run* [q]
    (fresh [item-name item-color not-country]
      (== q [item-name item-color])
      (items item-name item-color)
      (!= country not-country)

      (conda
        [(restricted-to country item-name)
         (conda
           [(not-allowed-in country item-name)
            fail]
           [succeed])]
        [(restricted-to not-country item-name)
         fail]
        ;; No entry in restricted-to for item-name
        [(not-allowed-in country item-name)
         fail]
        [succeed]))))
