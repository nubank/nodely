(ns nodely.engine.lazy
  (:refer-clojure :exclude [eval resolve])
  (:require
   [clojure.core.async :as async]
   [nodely.data :as data]
   [nodely.engine.core :as core]
   [nodely.engine.protocols :as engine.protocols]))

(deftype LazyEngine []
  engine.protocols/Engine
  (-eval [_engine env k _opts]
    (core/resolve k env))

  (-eval-key [engine env k opts]
    (data/get-value (engine.protocols/-eval engine env k opts) k))

  (-eval-key-channel [engine env k opts]
    (async/thread (engine.protocols/-eval-key engine env k opts))))

(defn eval
  [env k]
  (core/resolve k env))

(defn eval-key
  [env k]
  (data/get-value (eval env k) k))

(defn eval-key-channel
  [env k]
  (async/thread (eval-key env k)))

(defn eval-node
  [node env]
  (eval-key (assoc env ::target node) ::target))

(defn eval-node-with-values
  [node m]
  (eval-node node (data/values->env m)))
