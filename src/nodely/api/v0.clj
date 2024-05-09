(ns nodely.api.v0
  (:refer-clojure :exclude [cond eval sequence])
  (:require
   [nodely.data]
   [nodely.engine.applicative :as applicative]
   [nodely.engine.applicative.core-async :as applicative.core-async]
   [nodely.engine.applicative.promesa :as applicative.promesa]
   [nodely.engine.core :as engine-core]
   [nodely.engine.core-async.core]
   [nodely.engine.core-async.iterative-scheduling]
   [nodely.engine.core-async.lazy-scheduling]
   [nodely.engine.lazy]
   [nodely.engine.manifold]
   [nodely.syntax]
   [nodely.vendor.potemkin :refer [import-fn import-vars]]))

(import-vars nodely.syntax/>cond
             nodely.syntax/>if
             nodely.syntax/>leaf
             nodely.syntax/>and
             nodely.syntax/>or
             nodely.syntax/>value
             nodely.syntax/>sequence
             nodely.syntax/blocking
             nodely.engine.core-async.core/>channel-leaf
             nodely.data/value
             nodely.data/leaf
             nodely.data/sequence
             nodely.data/branch
             engine-core/checked-env)

(import-fn nodely.engine.lazy/eval-node-with-values eval-node-with-values)
(import-fn nodely.data/merge-values merge-values)
(import-fn nodely.data/get-value get-value)

(def engine-data
  {:core-async.lazy-scheduling      {::ns (find-ns 'nodely.engine.core-async.lazy-scheduling)
                                     ::opts-fn identity
                                     ::eval-key-channel true}
   :core-async.iterative-scheduling {::ns (find-ns 'nodely.engine.core-async.iterative-scheduling)
                                     ::opts-fn identity}
   :async.manifold                  {::ns (find-ns 'nodely.engine.manifold)
                                     ::opts-fn (constantly nil)}
   :applicative.promesa             {::ns (find-ns 'nodely.engine.applicative)
                                     ::opts-fn #(assoc % ::applicative/context applicative.promesa/context)}
   :applicative.core-async          {::ns (find-ns 'nodely.engine.applicative)
                                     ::opts-fn #(assoc % ::applicative/context applicative.core-async/context)
                                     ::eval-key-channel true}
   :sync.lazy                       {::ns (find-ns 'nodely.engine.lazy)
                                     ::opts-fn (constantly nil)
                                     ::eval-key-channel true}})

(defn- engine-fn
  [engine-name use]
  (ns-resolve (::ns (engine-data engine-name)) use))

(def engine-fn (memoize engine-fn))

(defn eval
  ([env k]
   (eval env k {}))
  ([env k {engine ::engine
           :or    {engine :core-async.lazy-scheduling}
           :as    opts}]

   (let [efn (engine-fn engine 'eval)]
     (if-let [opts ((::opts-fn (engine-data engine)) opts)]
       (efn env k opts)
       (efn env k)))))

(defn eval-key
  ([env k]
   (eval-key env k {}))
  ([env k {engine ::engine
           :or    {engine :core-async.lazy-scheduling}
           :as    opts}]
   (let [efn (engine-fn engine 'eval-key)]
     (if-let [opts ((::opts-fn (engine-data engine)) opts)]
       (efn env k opts)
       (efn env k)))))

(defn eval-key-channel
  ([env k]
   (eval-key-channel env k {}))
  ([env k {engine ::engine
           :or    {engine :core-async.lazy-scheduling}
           :as    opts}]
   (let [efn (engine-fn engine 'eval-key-channel)]
     (if-let [opts ((::opts-fn (engine-data engine)) opts)]
       (efn env k opts)
       (efn env k)))))

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
