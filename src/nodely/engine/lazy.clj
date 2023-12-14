(ns nodely.engine.lazy
  (:refer-clojure :exclude [eval resolve])
  (:require
   [clojure.core.async :as async]
   [nodely.data :as data]
   [nodely.engine.core :as core]))

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
