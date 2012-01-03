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

;; As a function

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

(get-items-colors-for-country 'US)
;=> ([Purse Blue] [Banana Yellow] [Car Red])

(get-items-colors-for-country 'UK)
;=> ([Purse Blue])

(get-items-colors-for-country 'France)
;=> ([Banana Yellow])

(get-items-colors-for-country 'Australia)
;=> ([Purse Blue] [Banana Yellow])


;; Or as a goal

(defn items-colors-for-country [country item-name item-color]
  (fresh [not-country]
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
      [succeed])))

(run* [q]
  (fresh [country item-name item-color]
    (== q [country item-name item-color])
    (conde
      [(== 'US country)]
      [(== 'UK country)]
      [(== 'France country)]
      [(== 'Australia country)])
    (items-colors-for-country country item-name item-color)))
;=> ([US Purse Blue] [UK Purse Blue] [US Banana Yellow] [US Car Red] [Australia Purse Blue] [Australia Banana Yellow] [France Banana Yellow])
