(ns nodely.engine.virtual-workers
  (:refer-clojure :exclude [eval])
  (:require
   [loom.alg :as alg]
   [nodely.data :as data]
   [nodely.engine.core :as core]
   [nodely.engine.virtual-future :as virtual-future]))

(declare eval-async)

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
        f      (::data/value (eval-async (::data/process-node node) future-env))
        in     (prepare-inputs [in-key] future-env)]
    (data/value (->> (get in in-key)
                     (mapv #(virtual-future/vfuture (f %)))
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
                             (let [node (core/get! env k)]
                               (assoc acc k (virtual-future/vfuture (eval-async node acc)))))
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
