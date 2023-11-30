(ns nodely.engine.applicative.protocols
  (:require
   [cats.context :as context]))

(defprotocol RunNode
  (-apply-fn [ctx f mv]))

(defn apply-fn
  [f mv]
  (let [ctx (context/infer mv)]
    (-apply-fn ctx f mv)))
