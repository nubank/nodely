(ns nodely.engine.schema
  (:require
   [cats.core :as m]
   [nodely.data :as data]
   [schema.core :as s]))

(defn validate
  [node value]
  (when-let [schema #nu/tapd (and (s/fn-validation?)
                         (:yielded-schema (meta node)))]
    #nu/tapd (s/validate schema #nu/tapd (::data/value value)))
  value)

(defn fvalidate
  [return node]
  (m/fmap #(validate node %) return))
