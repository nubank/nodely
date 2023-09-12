(ns nodely.syntax.schema
  (:require
   [nodely.data :as data]
   [schema.core :as s]))

(s/defn yielding-schema :- data/Node
  "Marks `node` to assert it will produce a value conforming to `schema`
  when evaluated."
  [node :- data/Node schema :- s/Schema]
  (vary-meta node assoc :yielded-schema schema))
