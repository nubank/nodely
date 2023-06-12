(ns nodely.engine.manifold
  (:refer-clojure :exclude [eval])
  (:require [loom.alg :as alg]
            [manifold.deferred :as deferred]
            [nodely.data :as data]
            [nodely.engine.core :as core]))

(defn- prepare-inputs
  [input-keys future-env]
  (->> (select-keys future-env input-keys)
       (map (juxt key (comp ::data/value deref val)))
       (into {})))

(defn eval-leaf
  [node future-env]
  (let [in (prepare-inputs (::data/inputs node) future-env)]
    (core/eval-leaf node in)))

(defn eval-sequence
  [node future-env]
  (let [in-key (::data/input node)
        f      (::data/fn node)
        in     (prepare-inputs [in-key] future-env)]
    (data/value (->> (get in in-key)
                     (mapv #(deferred/future (f %)))
                     (mapv deref)))))

(defn eval-async
  [node future-env]
  (case (::data/type node)
    :value    node
    :leaf     (eval-leaf node future-env)
    :sequence (eval-sequence node future-env)))

(defn eval-env
  "Env must have no branches"
  [env]
  (let [graph      (core/env->graph env)
        top-sort   (alg/topsort graph)
        future-env (reduce (fn [acc k]
                             (let [node (get env k)]
                               (assoc acc k (deferred/future (eval-async node acc)))))
                           {}
                           top-sort)]
    (into {} (map (juxt key (comp deref val)) future-env))))

(defn eval
  [env k]
  (let [new-env       (core/unbranch-all env)
        node-deps     (core/dependencies-for k new-env)
        env-with-deps (select-keys new-env (cons k node-deps))]
    (merge new-env (eval-env env-with-deps))))

(defn eval-key
  "Steps:
   1. Solves all branches synchronously
   2. Evaluates only required keys asynchronously"
  [env k]
  (data/get-value (eval env k) k))

(defn eval-node
  [node env]
  (eval-key (assoc env ::target node) ::target))
