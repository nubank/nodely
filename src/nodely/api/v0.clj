(ns nodely.api.v0
  (:refer-clojure :exclude [cond eval sequence])
  (:require
   [nodely.data]
   [nodely.engine.applicative.engine :as engine.applicative.engine]
   [nodely.engine.async.manifold-engine :as engine.async.manifold-engine]
   [nodely.engine.async.virtual-futures-engine :as engine.async.virtual-futures-engine]
   [nodely.engine.core :as engine-core]
   [nodely.engine.core-async.iterative-scheduling-engine :as engine.core-async.iterative-scheduling-engine]
   [nodely.engine.core-async.lazy-scheduling-engine :as engine.core-async.lazy-scheduling-engine]
   [nodely.engine.lazy :as engine.lazy]
   [nodely.engine.protocols :as engine.protocols]
   [nodely.syntax :as syntax]
   [nodely.vendor.potemkin :refer [import-fn import-vars]]))

(import-vars nodely.syntax/>cond
             nodely.syntax/>if
             nodely.syntax/>leaf
             nodely.syntax/>and
             nodely.syntax/>or
             nodely.syntax/>value
             nodely.syntax/>sequence
             nodely.syntax/blocking
             nodely.data/value
             nodely.data/leaf
             nodely.data/sequence
             nodely.data/branch
             nodely.data/with-try
             engine-core/checked-env)

(import-fn nodely.engine.lazy/eval-node-with-values eval-node-with-values)
(import-fn nodely.data/merge-values merge-values)
(import-fn nodely.data/get-value get-value)

(def core-async-failure
  (delay
   (try (require 'nodely.engine.applicative.core-async
                 'nodely.engine.core-async.core
                 'nodely.engine.core-async.iterative-scheduling
                 'nodely.engine.core-async.lazy-scheduling)
        (catch Exception e
          {:msg                   "Could not locate core-async on classpath."
           ::error                :missing-ns
           ::requested-namespaces '[nodely.engine.applicative.core-async
                                    nodely.engine.core-async.core
                                    nodely.engine.core-async.iterative-scheduling
                                    nodely.engine.core-async.lazy-scheduling]
           :cause                 e}))))

(def engine-data
  {:core-async.lazy-scheduling      engine.core-async.lazy-scheduling-engine/->CoreAsyncLazySchedulingEngine
   :core-async.iterative-scheduling engine.core-async.iterative-scheduling-engine/->CoreAsyncIterativeSchedulingEngine
   :async.manifold                  engine.async.manifold-engine/->AsyncManifoldEngine
   :applicative.promesa             engine.applicative.engine/->promesa-applicative-engine
   :applicative.core-async          engine.applicative.engine/->core-async-applicative-engine
   :sync.lazy                       engine.lazy/->LazyEngine
   :async.virtual-futures           engine.async.virtual-futures-engine/->AsyncVirtualFuturesEngine
   :applicative.virtual-future      engine.applicative.engine/->virtual-future-applicative-engine})

(defmacro >channel-leaf
  [expr]
  (let [symbols-to-be-replaced (#'syntax/question-mark-symbols expr)
        fn-expr                (#'syntax/fn-with-arg-map symbols-to-be-replaced expr)]
    (if-let [{:keys [msg cause] :as enable-failure} @core-async-failure]
      (throw (ex-info msg (dissoc enable-failure :msg :cause) cause))
      (list `nodely.engine.core-async.core/channel-leaf
            (mapv #'syntax/question-mark->keyword symbols-to-be-replaced)
            fn-expr))))

(defn- protocol-engine
  "Instantiates the protocol engine registered under `engine-name` and, via its
  `-enable-deref`, verifies it can run on the current classpath -- throwing an
  informative error otherwise. Returns the ready-to-use engine instance."
  [engine-name]
  (if-let [engine-constructor (engine-data engine-name)]
    (let [engine (engine-constructor)]
      (when-let [{:keys [msg cause] :as enable-failure} @(engine.protocols/-enable-deref engine)]
        (throw (ex-info msg
                        (-> enable-failure
                            (dissoc :msg :cause)
                            (assoc ::specified-engine-name engine-name))
                        cause)))
      engine)
    (throw (ex-info "Unsupported engine specified, please specify a supported engine."
                    {:specified-engine-name engine-name
                     :supported-engine-names (set (keys engine-data))}))))

(defn eval
  ([env k]
   (eval env k {}))
  ([env k {engine-name ::engine
           :or         {engine-name :core-async.lazy-scheduling}
           :as         opts}]
   (let [engine           (protocol-engine engine-name)]
     (engine.protocols/eval engine env k (engine.protocols/-prepare-opts engine opts)))))

(defn eval-key
  ([env k]
   (eval-key env k {}))
  ([env k {engine-name ::engine
           :or         {engine-name :core-async.lazy-scheduling}
           :as         opts}]
   (let [engine           (protocol-engine engine-name)]
     (engine.protocols/eval-key engine env k (engine.protocols/-prepare-opts engine opts)))))

(defn eval-key-channel
  ([env k]
   (eval-key-channel env k {}))
  ([env k {engine-name ::engine
           :or         {engine-name :core-async.lazy-scheduling}
           :as         opts}]
   (let [engine           (protocol-engine engine-name)]
     (engine.protocols/eval-key-channel engine env k (engine.protocols/-prepare-opts engine opts)))))

(defn eval-node
  ([env node]
   (eval-node env node {}))
  ([env node opts]
   (eval-key (assoc env ::target node) ::target opts)))

(defn eval-node-channel
  ([env node]
   (eval-node-channel env node {}))
  ([env node opts]
   (eval-key-channel (assoc env ::target node) ::target opts)))
