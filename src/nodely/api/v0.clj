(ns nodely.api.v0
  (:refer-clojure :exclude [cond eval sequence])
  (:require
   [nodely.data]
   [nodely.engine.applicative :as applicative]
   [nodely.engine.applicative.promesa :as applicative.promesa] ;; promesa
   [nodely.engine.core :as engine-core]
   [nodely.engine.lazy]
   [nodely.engine.manifold]
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
             engine-core/checked-env)

(import-fn nodely.engine.lazy/eval-node-with-values eval-node-with-values)
(import-fn nodely.data/merge-values merge-values)
(import-fn nodely.data/get-value get-value)

(def virtual-future-failure
  (delay
    (try (import java.util.concurrent.ThreadPerTaskExecutor)
         (require '[nodely.engine.virtual-workers]
                  '[nodely.engine.applicative.virtual-future :as applicative.virtual-future])
         (catch Exception e
           {:msg              "Classloader could not locate `java.util.concurrent.ThreadPerTaskExecutor`, virtual futures require JDK 21 or higher."
            ::error           :missing-class
            ::requested-class "java.util.concurrent.ThreadPerTaskExecutor"
            :cause            e}))))

(def core-async-failure
  (delay
    (try (do (require '[nodely.engine.applicative.core-async :as applicative.core-async]
                      '[nodely.engine.core-async.core]
                      '[nodely.engine.core-async.iterative-scheduling]
                      '[nodely.engine.core-async.lazy-scheduling])
             (import-vars))
         (catch Exception e
           {:msg                   "Could not locate core-async on classpath."
            ::error                :missing-ns
            ::requested-namespaces '[nodely.engine.core-async.core
                                     nodely.engine.core-async.iterative-scheduling
                                     nodely.engine.core-async.lazy-scheduling]
            :cause                 e}))))

(defn manifold-failure
  [])

(defn promesa-failure
  [])

(def engine-data
  {:core-async.lazy-scheduling      {::ns-name          'nodely.engine.core-async.lazy-scheduling
                                     ::opts-fn          identity
                                     ::enable-deref     core-async-failure
                                     ::eval-key-channel true}
   :core-async.iterative-scheduling {::ns-name          'nodely.engine.core-async.iterative-scheduling
                                     ::opts-fn          identity
                                     ::enable-deref     core-async-failure}
   :async.manifold                  {::ns-name          'nodely.engine.manifold
                                     ::opts-fn          (constantly nil)
                                     ::enable-deref     (delay nil)}
   :applicative.promesa             {::ns-name          'nodely.engine.applicative
                                     ::opts-fn          #(assoc % ::applicative/context
                                                                (var-get (resolve 'nodely.engine.applicative.promesa/context)))
                                     ::enable-deref     (delay nil)}
   :applicative.core-async          {::ns-name          'nodely.engine.applicative
                                     ::opts-fn          #(assoc % ::applicative/context
                                                                (var-get (resolve 'nodely.engine.applicative.core-async/context)))
                                     ::eval-key-channel true
                                     ::enable-deref     core-async-failure}
   :sync.lazy                       {::ns-name          'nodely.engine.lazy
                                     ::opts-fn          (constantly nil)
                                     ::eval-key-channel true
                                     ::enable-deref     (delay nil)}
   :async.virtual-futures           {::ns-name          'nodely.engine.virtual-workers
                                     ::opts-fn          (constantly nil)
                                     ::eval-key-channel true
                                     ::enable-deref     virtual-future-failure}
   :applicative.virtual-future      {::ns-name          'nodely.engine.applicative
                                     ::opts-fn          #(assoc % ::applicative/context
                                                                (var-get (resolve 'nodely.engine.applicative.virtual-future/context)))
                                     ::eval-key-channel true
                                     ::enable-deref     virtual-future-failure}})

(defmacro >channel-leaf
  [expr]
  (let [symbols-to-be-replaced (#'syntax/question-mark-symbols expr)
        fn-expr                (#'syntax/fn-with-arg-map symbols-to-be-replaced expr)]
    (if-let [{:keys [msg cause] :as enable-failure} @core-async-failure]
      (throw (ex-info msg (dissoc enable-failure :msg :cause) cause))
      (list `nodely.engine.core-async.core/channel-leaf
          (mapv #'syntax/question-mark->keyword symbols-to-be-replaced)
          fn-expr))))

(defn- engine-fn
  [engine-name use]
  (if-let [engine-data (engine-data engine-name)]
    (if-let [{:keys [msg cause] :as enable-failure} @(::enable-deref engine-data)]
      (throw (ex-info msg
                      (-> enable-failure
                          (dissoc :msg :cause)
                          (assoc ::specified-engine-name engine-name))
                      cause))
      (ns-resolve (find-ns (::ns-name engine-data)) use))
    (throw (ex-info "Unsupported engine specified, please specify a supported engine."
                    {:specified-engine-name engine-name
                     :supported-engine-names (set (keys engine-data))}))))

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
