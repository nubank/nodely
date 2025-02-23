(ns nodely.engine.applicative
  (:refer-clojure :exclude [eval sequence])
  (:require
   [clojure.core.async :as async]
   [clojure.pprint :as pp]
   [nodely.data :as data]
   [nodely.engine.applicative.core :as app]
   [nodely.engine.applicative.protocols :as protocols]
   [nodely.engine.core :as core]
   [nodely.engine.lazy-env :as lazy-env]))

(prefer-method pp/simple-dispatch clojure.lang.IPersistentMap clojure.lang.IDeref)

(defn in-context?
  [x context]
  (= context (protocols/-get-context x)))

(declare eval-node)

(defn eval-sequence
  [node lazy-env {::keys [context] :as opts}]
  (let [in-key (::data/input node)
        mf     (app/fmap ::data/value
                         (eval-node (::data/process-node node) lazy-env opts))
        mseq   (get lazy-env in-key)]
    (->> mseq
         (app/fmap (comp (partial app/sequence context)
                         (partial map
                                  (comp (partial app/fapply mf)
                                        (partial app/pure context)))
                         ::data/value))
         app/join
         (app/fmap data/value))))

(defn eval-branch
  [{::data/keys [condition truthy falsey]}
   lazy-env
   opts]
  (app/bind (eval-node condition lazy-env opts)
            (fn [condition-value-node]
              (if (:nodely.data/value condition-value-node)
                (eval-node truthy lazy-env opts)
                (eval-node falsey lazy-env opts)))))

(defn noop-validate
  [return _]
  return)

(defn eval-leaf
  [leaf lazy-env {::keys [context]}]
  (let [deps-keys (seq (::data/inputs leaf))
        tags      (::data/tags leaf)
        f         (with-meta (::data/fn leaf)
                    {::data/tags tags})]
    (app/mlet [v (app/apply-fn f (app/fmap #(core/prepare-inputs deps-keys (zipmap deps-keys %))
                                           (app/sequence context (mapv #(get lazy-env %) deps-keys))))]
              (if (in-context? v context)
                (app/fmap data/value v)
                (app/pure context (data/value v))))))

(defn eval-node
  [node lazy-env opts]
  (case (::data/type node)
    :value    (app/pure (::context opts) node)
    :leaf     (eval-leaf node lazy-env opts)
    :branch   (eval-branch node lazy-env opts)
    :sequence (eval-sequence node lazy-env opts)))

(defn eval-in-context
  [env k lazy-env {::keys [fvalidate] :as opts}]
  (let [node     (get env k)
        node-ret (eval-node node lazy-env opts)
        validator (or fvalidate noop-validate)]
    (validator node-ret node)))

(defn eval-key
  [env k opts]
  (let [lazy-env (lazy-env/lazy-env env eval-in-context opts)]
    (::data/value (protocols/-extract (get lazy-env k)))))

(defn eval-key-contextual
  [env k opts]
  (let [lazy-env (lazy-env/lazy-env env eval-in-context opts)]
    (app/fmap ::data/value (get lazy-env k))))

(defn eval-key-channel
  [env k opts]
  (let [contextual-v (eval-key-contextual env k opts)
        chan         (async/promise-chan)]
    (app/fmap (partial async/put! chan) contextual-v)
    chan))

(defn eval
  [env k opts]
  (let [lazy-env (lazy-env/lazy-env env eval-in-context opts)]
    (protocols/-extract (get lazy-env k)) ;; ensures k is resolved
    (merge env
           (reduce (fn [acc [k v]] (assoc acc k (protocols/-extract v)))
                   {}
                   (lazy-env/scheduled-nodes lazy-env)))))
