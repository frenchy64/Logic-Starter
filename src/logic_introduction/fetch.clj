(ns logic-introduction.fetch
  (:refer-clojure :exclude [inc reify ==])
  (:use [clojure.data.json :only (json-str write-json read-json)])
  (:use [clojure.core.logic minikanren prelude nonrel match disequality]))

;; http://www.daddymodern.com/useful-prolog/

(defn url-to-process [url]
  (== url "http://api.worldbank.org/countries/USA/indicators/AG.AGR.TRAC.NO?per_page=10&date=2005:2011&format=json"))

(defn slurpo [url datastream]
  "Input: url
  Output: datastream"
  (project [url]
    (== datastream (slurp url))))

(defn read-jsono [input output]
  (project [input]
    (== output (read-json input))))


(defn fetch-data [fetched-data]
  (exist [url datastream]
    (url-to-process url)
    (slurpo url datastream)
    (read-jsono datastream fetched-data)))

(defn process-data-header [header]
  (matche [header]
          ([?ignore])))

(defn json-object-has-value [json-object name value]
  (project [json-object name]
    (== [name value] (find json-object name))))

(defn process-json-object [json-object]
  (exist [date-value indicator-value temp]
    (json-object-has-value json-object :date date-value)
    (json-object-has-value json-object :value indicator-value)
    (project [date-value indicator-value]
             ;; TODO probably have to wrap println
             (== temp (println "Date: " date-value))
             (== temp (println "Value: " indicator-value))
             (== temp (println)))))

(defn process-data-contents [json]
  (matche [json]
          ([[]])
          ([[?json-object . ?rest]]
           (process-json-object ?json-object)
           (process-data-contents ?rest))))

(defn process-data [json]
  (matche [json]
          ([[?header . ?contents]] 
            (process-data-header ?header)
            (process-data-contents ?contents))))

(defn fetch-and-process-data []
  (run* [fetched-data]
        (fetch-data fetched-data)
        (process-data fetched-data)))
