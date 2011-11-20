(ns logic-introduction.database
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

;; Define a relation. Relational Database tables are modeled after relations.
;; Think of a database table called person, with 3 columns (id, name, age)
(defrel person id name age)

;; Add a couple of people
(fact person 1 "Ambrose" 21)
(fact person 2 "Anthony" 17)
(fact person 3 "Miran" 17)
(fact person 4 "Gerald" 55)

;; select-from-person queries the person relation. (ignore implementation)
(defn select-from-person [id name age]
  (run* [q]
        (== q {:id id :age age :name name})
        (person id name age)))

;; Ask for the rows where id=1, name=Ambrose, age=21
(let [id 1
      name "Ambrose"
      age 21]
  (select-from-person id name age))
;=> ({:id 1, :age 21, :name "Ambrose"})

;; Ask for the rows where id=2, name=Anthony, age=17
(let [id 2
      name "Anthony"
      age 17]
  (select-from-person id name age))
;=> ({:id 2, :age 17, :name "Anthony"})


;; We can use a logic variable to specify a wildcard.
;; Ask for all the rows where age=17, and don't care about the value of id or name
(let [id (lvar) 
      name (lvar)
      age 17]
  (select-from-person id name age))
;=> ({:id 3, :age 17, :name "Miran"} 
;    {:id 2, :age 17, :name "Anthony"})

;; Ask for all the rows
(let [id (lvar) 
      name (lvar)
      age (lvar)]
  (select-from-person id name age))
;=> ({:id 1, :age 21, :name "Ambrose"} 
;    {:id 3, :age 17, :name "Miran"} 
;    {:id 4, :age 55, :name "Gerald"} 
;    {:id 2, :age 17, :name "Anthony"})



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Create another relation for occupations

(defrel occupation id location title)

(fact occupation 1 "University of Western Australia" "Student")
(fact occupation 2 "Clojure shop" "Clojurer")
(fact occupation 3 "Ruby shop" "Ruby guy")


(defn select-from-occupation [id location title]
  (run* [q]
        (== q {:id id :location location :title title})
        (occupation id location title)))

;; Ask for all the occupations rows
(let [id (lvar) 
      location (lvar)
      title (lvar)]
  (select-from-occupation id location title))
;=> ({:id 1, :location "University of Western Australia", :title "Student"} 
;    {:id 3, :location "Ruby shop", :title "Ruby guy"} 
;    {:id 2, :location "Clojure shop", :title "Clojurer"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Define a relation to join the person and occupation rows by id
(defrel join-person-and-occupation person-id occ-id)

(fact join-person-and-occupation 1 1) ;; Ambrose is a student at UWA
(fact join-person-and-occupation 2 2) ;; Anthony is a Clojure guy
(fact join-person-and-occupation 3 2) ;; Miran is a Ruby guy
(fact join-person-and-occupation 4 2) ;; Gerald is a Ruby guy
(fact join-person-and-occupation 4 3) ;; Gerald is a Clojure guy


(defn select-from-person-and-occupation [person-id name age occ-id location title]
  (run* [q]
        (== q {:person-id person-id :age age :name name
               :occ-id occ-id :location location :title title})
        (person person-id name age)
        (occupation occ-id location title)
        (join-person-and-occupation person-id occ-id)))

(let [person-id (lvar)
      name (lvar)
      age (lvar)
      occ-id (lvar)
      location (lvar)
      title (lvar)]
  (select-from-person-and-occupation person-id name age occ-id location title))
;=> ({:person-id 1, :age 21, :name "Ambrose", :occ-id 1, :location "University of Western Australia", :title "Student"} 
;    {:person-id 3, :age 17, :name "Miran", :occ-id 2, :location "Clojure shop", :title "Clojurer"} 
;    {:person-id 4, :age 55, :name "Gerald", :occ-id 2, :location "Clojure shop", :title "Clojurer"} 
;    {:person-id 2, :age 17, :name "Anthony", :occ-id 2, :location "Clojure shop", :title "Clojurer"}
;    {:person-id 4, :age 55, :name "Gerald", :occ-id 2, :location "Clojure shop", :title "Clojurer"})
