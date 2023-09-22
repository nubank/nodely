(ns nodely.engine.applicative
  (:refer-clojure :exclude [eval sequence])
  (:require
   [cats.context :as context]
   [cats.core :as m]
   [clojure.core.async :as async]
   [clojure.pprint :as pp]
   [nodely.data :as data]
   [nodely.engine.applicative.promesa :as promesa]
   [nodely.engine.core :as core]
   [nodely.engine.core-async.core :as core-async.core]
   [nodely.engine.lazy-env :as lazy-env]))

(prefer-method pp/simple-dispatch clojure.lang.IPersistentMap clojure.lang.IDeref)

;; sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
(defn sequence
  [chs]
  (reduce (m/lift-a 2 conj) (m/pure []) chs))

(defn in-context?
  [x context]
  (and (satisfies? cats.protocols/Contextual x)
       (= context (cats.protocols/-get-context x))))

(declare eval-node)

(defn eval-sequence
  [node lazy-env opts]
  (let [f  (::data/fn node)
        in-key (::data/input node)]
    (m/alet [input-seq (get lazy-env in-key)
             result (sequence (map (fn [x] (m/fmap f (m/pure x)))
                                   (::data/value input-seq)))]
            (data/value result))))

(defn eval-branch
  [{::data/keys [condition truthy falsey]}
   lazy-env
   opts]
  (m/alet [condition-value-node (eval-node condition lazy-env opts)
           result (if (::data/value condition-value-node)
                    (eval-node truthy lazy-env opts)
                    (eval-node falsey lazy-env opts))]
          result))

(defn- apply-f
  [context f deps-keys deps]
  (let [in     (core/prepare-inputs deps-keys (zipmap deps-keys deps))]
    (if (instance? nodely.engine.core_async.core.AsyncThunk f)
      (let [exception-ch (async/promise-chan)
            result-ch    (core-async.core/evaluation-channel f in {:exception-ch exception-ch})]
        (async/merge [exception-ch result-ch]))
      (let [result (f in)]
        (if (in-context? result context)
          (m/fmap data/value result)
          (m/pure context (data/value result)))))))

(defn noop-validate
  [return _]
  return)

(defn eval-leaf
  [leaf lazy-env {::keys [context]}]
  (let [deps-keys (seq (::data/inputs leaf))
        f         (::data/fn leaf)]
    (m/alet [deps (sequence (mapv #(get lazy-env %) deps-keys))
             result (apply-f context f deps-keys deps)]
            result)))

(defn eval-node
  [node lazy-env opts]
  (case (::data/type node)
    :value    (m/pure node)
    :leaf     (eval-leaf node lazy-env opts)
    :branch   (eval-branch node lazy-env opts)
    :sequence (eval-sequence node lazy-env opts)))

(defn eval-in-context
  [env k lazy-env {::keys [fvalidate] :as opts}]
  (context/with-context (::context opts)
    (let [node     (get env k)
          node-ret (eval-node node lazy-env opts)
          validator (or fvalidate noop-validate)]
      (validator node-ret node))))

(defn eval-key
  ([env k]
   (eval-key env k {}))
  ([env k opts]
   (let [opts (merge {::context promesa/context} opts)
         lazy-env (lazy-env/lazy-env env eval-in-context opts)]
     (::data/value (m/extract (get lazy-env k))))))

(defn eval
  ([env k]
   (eval env k {::context promesa/context}))
  ([env k opts]
   (let [lazy-env (lazy-env/lazy-env env eval-in-context opts)]
     (m/extract (get lazy-env k)) ;; ensures k is resolved
     (merge env
            (reduce (fn [acc [k v]] (assoc acc k (m/extract v)))
                    {}
                    (lazy-env/scheduled-nodes lazy-env))))))
