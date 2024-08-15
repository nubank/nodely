(ns nodely.engine.schema
  (:require
   [nodely.data :as data]
   [nodely.engine.applicative.core :as app]
   [schema.core :as s]))

(defn validate
  [node value]
  (when-let [schema (and (s/fn-validation?)
                         (:yielded-schema (meta node)))]
    (s/validate schema (::data/value value)))
  value)

(defn fvalidate
  [return node]
  (app/fmap #(validate node %) return))
