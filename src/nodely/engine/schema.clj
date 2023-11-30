(ns nodely.engine.schema
  (:require
   [cats.core :as m]
   [nodely.data :as data]
   [schema.core :as s]))

(defn validate
  [node value]
  (when-let [schema (and (s/fn-validation?)
                         (:yielded-schema (meta node)))]
    (s/validate schema (::data/value value)))
  value)

(defn fvalidate
  [return node]
  (m/fmap #(validate node %) return))
