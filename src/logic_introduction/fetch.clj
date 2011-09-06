(ns logic-introduction.fetch
  (:refer-clojure :exclude [inc reify ==])
  (:use [clojure.data.json :only (json-str write-json read-json)])
  (:use [clojure.core.logic minikanren prelude nonrel match disequality]))

;; http://www.daddymodern.com/useful-prolog/

(def url-to-process "http://api.worldbank.org/countries/USA/indicators/AG.AGR.TRAC.NO?per_page=10&date=2005:2011&format=json")

(defn slurpo [url datastream]
  (project [url]
    (== datastream (slurp url))))

(defn read-jsono [input output]
  (project [input]
    (== output (read-json input))))

(defn fetch-data [fetched-data]
  (exist [datastream]
    (slurpo url-to-process datastream)
    (read-jsono datastream fetched-data)))

(defn process-data-header [header]
  (matche [header]
          ([?ignore])))

;; TODO how to pattern match a map? Turn into seq..
(defn json-object-has-value [json-object name value]
  (matche [json-object]
          ([[[name value] . ?rest]]
           )))

(defn process-json-object [json-object]
  (exist [date datevalue value indicator-value]
    (json-object-has-value json-object date datevalue)
    (json-object-has-value json-object value indicator-value)
    (project [date-value indicator-value]
             (println "Date: " date-value)
             (println "Value: " indicator-value)
             (println))))

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
