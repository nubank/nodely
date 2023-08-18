(ns nodely.engine.applicative
  (:refer-clojure :exclude [eval])
  (:require
   [cats.context :as context]
   [cats.core :as m]
   [clojure.pprint :as pp]
   [nodely.data :as data]
   [nodely.engine.applicative.promesa :as promesa]
   [nodely.engine.core :as core]
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
  (let [in (core/prepare-inputs deps-keys (zipmap deps-keys deps))
        result (f in)]
    (if (in-context? result context)
      (m/fmap data/value result)
      (m/pure context (data/value result)))))

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

(defn eval-promise
  [env k lazy-env opts]
  (context/with-context (::context opts)
    (let [node (get env k)]
      (eval-node node lazy-env opts))))

(defn eval-key
  ([env k]
   (eval-key env k {::context promesa/context}))
  ([env k opts]
   (let [lazy-env (lazy-env/lazy-env env eval-promise opts)]
     (::data/value (m/extract (get lazy-env k))))))

(defn eval
  ([env k]
   (eval env k {::context promesa/context}))
  ([env k opts]
   (let [lazy-env (lazy-env/lazy-env env eval-promise opts)]
     (m/extract (get lazy-env k)) ;; ensures k is resolved
     (merge env
            (reduce (fn [acc [k v]] (assoc acc k (m/extract v)))
                    {}
                    (lazy-env/scheduled-nodes lazy-env))))))
