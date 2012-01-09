(ns logic-introduction.fetch
  (:refer-clojure :exclude [==])
  (:use [clojure.data.json :only (json-str write-json read-json)])
  (:use [clojure.core.logic]))

;; http://www.daddymodern.com/useful-prolog/

(defn url-to-process 
  "Output: url"
  [url]
  (== url "http://api.worldbank.org/countries/USA/indicators/AG.AGR.TRAC.NO?per_page=10&date=2005:2011&format=json"))

(defn slurpo 
  "Input: url
  Output: datastream"
  [url datastream]
  (project [url]
    (== datastream (slurp url))))

(defn read-jsono 
  "Input: input
  Output: output"
  [input output]
  (project [input]
    (== output (read-json input))))


(defn fetch-data 
  "Output: fetched-data"
  [fetched-data]
  (fresh [url datastream]
    (url-to-process url)
    (slurpo url datastream)
    (read-jsono datastream fetched-data)))

(defn process-data-header 
  "Input: header"
  [header]
  (matche [header]
          ([?ignore])))

(defn json-object-has-value 
  "Input: json-object, name
  Output: value"
  [json-object name value]
  (project [json-object name]
    (== [name value] (find json-object name))))

(defn process-json-object 
  "Input: json-object"
  [json-object]
  (fresh [date-value indicator-value temp]
    (json-object-has-value json-object :date date-value)
    (json-object-has-value json-object :value indicator-value)
    (project [date-value indicator-value]
             ;; TODO probably have to wrap println
             (== temp (println "Date: " date-value))
             (== temp (println "Value: " indicator-value))
             (== temp (println)))))

(defn process-data-contents 
  "Input: json"
  [json]
  (matche [json]
          ([[]])
          ([[?json-object . ?rest]]
           (process-json-object ?json-object)
           (process-data-contents ?rest))))

(defn process-data 
  "Input: json"
  [json]
  (matche [json]
          ([[?header ?contents]] 
            (process-data-header ?header)
            (process-data-contents ?contents))))

(defn fetch-and-process-data []
  (run* [fetched-data]
        (fetch-data fetched-data)
        (process-data fetched-data)))
