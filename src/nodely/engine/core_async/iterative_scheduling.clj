(ns nodely.engine.core-async.iterative-scheduling
  (:refer-clojure :exclude [eval])
  (:require
   [clojure.core.async :as async]
   [clojure.set :as set]
   [nodely.data :as data]
   [nodely.engine.core :as core]
   [nodely.engine.core-async.core :refer [<? go-future]]))

(defn eval-leaf
  [node resolved-env]
  (let [in (core/prepare-inputs (data/node-inputs node) resolved-env)]
    (core/eval-leaf node in)))

(defn eval-sequence
  [node resolved-env {::keys [max-sequence-parallelism]
                      :or    {max-sequence-parallelism 4}}]
  (let [in-key          (::data/input node)
        f               (::data/value (first (core/node->value (::data/process-node node) resolved-env)))
        ;; TODO: add sequences to the test env
        sequence        (map data/value (core/get! (core/prepare-inputs [in-key] resolved-env) in-key))
        in-chan         (async/to-chan! sequence)
        pipeline-result (async/chan)]
    (async/pipeline max-sequence-parallelism
                    pipeline-result
                    (map (comp data/value f ::data/value))
                    in-chan)
    (async/go (data/value (map ::data/value (async/<! (async/into [] pipeline-result)))))))

(defn deps-satisfied?
  [node resolved-env]
  (let [deps (data/node-inputs node)]
    (set/subset? deps (set (keys resolved-env)))))

(defn partially-eval-branch
  [{::data/keys [condition truthy falsey]
    :as         node} resolved-env]
  (case (::data/type condition)
    :value    (if (::data/value condition)
                truthy
                falsey)
    :leaf     (if (::data/value (eval-leaf condition resolved-env))
                truthy
                falsey)
    :branch   (assoc node ::data/condition (partially-eval-branch condition resolved-env))
    :sequence (throw (ex-info "core.async engine does not currently support sequence as condition of a branch"
                              {::subject node}))))

(defn tentatively-eval-node
  [node resolved-env]
  (case (::data/type node)
    :value node
    :leaf (if (deps-satisfied? node resolved-env)
            (eval-leaf node resolved-env)
            node)
    :branch (if (deps-satisfied? node resolved-env)
              (recur (partially-eval-branch node resolved-env) resolved-env)
              node)
    :sequence node ;; we're choosing not to eval sequence at this step
    ))

(defn eval-branch
  [node resolved-env]
  (tentatively-eval-node (partially-eval-branch node resolved-env) resolved-env))

(defn eval-async
  "Given a `node` and a resolved environment `resolved-env`, returns a
  channel which will have the value of evaluating `node` in the
  environment."
  [node resolved-env opts]
  (case (::data/type node)
    :value    (go-future node)
    :leaf     (go-future (eval-leaf node resolved-env))
    :sequence (eval-sequence node resolved-env opts)
    :branch   (go-future (eval-branch node resolved-env))))

(defn eval-env
  "Env must have no branches"
  [env opts]
  (let [cur-keys     (keys env)
        env+channels (->> cur-keys
                          (map (fn [k] [k (async/promise-chan)]))
                          (into {}))]
    (doseq [k cur-keys]
      (async/go
        (let [deps       (seq (core/dependencies-for k env))
              deps-chans (mapv env+channels deps)
              values     (zipmap deps (<? (async/map vector deps-chans)))]
          (async/>! (env+channels k)
                    (<? (eval-async (env k) values opts))))))
    (into {} (map (juxt key (comp async/<!! val)) env+channels))))

(defn eval
  ([env k]
   (eval env k {::max-sequence-parallelism 4}))
  ([env k opts]
   (let [paths+envs (core/all-paths-for-node k env)]
     (if (= 1 (count paths+envs))
       (let [node-deps     (core/dependencies-for k env)
             env-with-deps (into {} (for [my-key (cons k node-deps)]
                                      [my-key (core/get! env my-key)]))]
         (merge env (eval-env env-with-deps opts)))
       (let [node-deps     (core/committed-dependencies k env)
             env-with-deps (select-keys env node-deps)
             env-result    (eval-env env-with-deps opts)]
         (recur (merge env env-result) k opts))))))

(defn eval-key
  ([env k]
   (eval-key env k {::max-sequence-parallelism 4}))
  ([env k opts]
   (data/get-value (eval env k opts) k)))

(defn eval-node
  [node env]
  (eval-key (assoc env ::target node) ::target))
