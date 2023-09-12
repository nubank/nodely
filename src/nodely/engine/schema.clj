(ns nodely.engine.schema
  (:require
   [cats.core :as m]
   [nodely.data :as data]
   [schema.core :as s]))

(defn validate
  [node value]
  (if-let [schema (and (s/fn-validation?)
                       (:yielded-schema (meta node)))]
    (data/value (s/validate schema (::data/value value)))
    value))

(defn fvalidate
  [return node]
  (m/fmap #(validate node %) return))
