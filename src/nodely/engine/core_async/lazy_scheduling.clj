(ns nodely.engine.core-async.lazy-scheduling
  (:refer-clojure :exclude [eval])
  (:require
   [clojure.core.async :as async]
   [nodely.data :as data]
   [nodely.engine.core :as core]
   [nodely.engine.core-async.core :as nodely.async]
   [nodely.engine.lazy-env :as lazy-env])
  (:import
   nodely.engine.lazy_env.LazySchedulingEnvironment))

(declare eval-async)

(defn eval-sequence
  [node
   sequence-input-promise-chan
   {::keys [max-sequence-parallelism out-ch exception-ch]
    :or {max-sequence-parallelism 4}}]
  (nodely.async/jog
   (let [f               (::data/fn node)
         sequence        (map data/value (::data/value (async/<! sequence-input-promise-chan)))
         in-chan         (async/to-chan! sequence)
         pipeline-result (async/chan)]
     (async/pipeline-async max-sequence-parallelism
                           pipeline-result
                           (fn [v ch]
                             (nodely.async/jog
                              (async/>! ch (nodely.async/<user-request
                                            (nodely.async/evaluation-channel
                                             f
                                             (::data/value v)
                                             {:exception-ch exception-ch
                                              :blocking (-> node
                                                            ::data/tags
                                                            ::data/blocking)})
                                            exception-ch))
                              (async/close! ch)))
                           in-chan)
     (async/>! out-ch
               (data/value (map ::data/value
                                (nodely.async/<request (async/into [] pipeline-result)
                                                       exception-ch)))))))

(defn eval-branch
  [{::data/keys [condition truthy falsey]}
   lazy-env
   {::keys [exception-ch] :as opts}]
  (nodely.async/jog
   (let [condition-value-node (nodely.async/<request (eval-async condition lazy-env (dissoc opts ::out-ch))
                                                     exception-ch)]
     (if (::data/value condition-value-node)
       (eval-async truthy lazy-env opts)
       (eval-async falsey lazy-env opts)))))

(defn eval-async
  "Given a `node` and a resolved environment `resolved-env`, returns a
  channel which will have the value of evaluating `node` in the
  environment."
  [node lazy-env {::keys [exception-ch out-ch]
                  :or    {out-ch (async/chan 1)}
                  :as opts}]
  (let [opts (assoc opts ::out-ch out-ch)]
    (case (::data/type node)
      :value    (async/put! out-ch node)
      :leaf     (nodely.async/jog
                 (let [deps       (seq (::data/inputs node))
                       deps-chans (mapv #(get lazy-env %) deps)
                       values     (zipmap deps (nodely.async/<request (async/map vector deps-chans) exception-ch))
                       in         (core/prepare-inputs (::data/inputs node) values)]
                   (async/>! out-ch
                             (nodely.async/<request (nodely.async/evaluation-channel
                                                     (::data/fn node) in {:exception-ch exception-ch
                                                                          :blocking (-> node
                                                                                        ::data/tags
                                                                                        ::data/blocking)})
                                                    exception-ch))))
      :sequence (let [dep  (::data/input node)
                      chan (get lazy-env dep)]
                  (eval-sequence node chan opts))
      :branch   (eval-branch node lazy-env opts)))
  out-ch)

(defn eval-node-promise
  [env k lazy-env opts]
  (let [ret (async/promise-chan)]
    (eval-async (get env k) lazy-env (assoc opts ::out-ch ret))
    ret))

(defn pull-env
  [env opts]
  (lazy-env/lazy-env env
                     eval-node-promise
                     (assoc opts
                            ::lazy-env/exception-fn
                            (fn [lazy-env k ex]
                              (async/put! (::exception-ch opts)
                                          ex)))))

(defn materialize
  [^LazySchedulingEnvironment pull-env {::keys [exception-ch]}]
  (let [scheduled-promises (lazy-env/scheduled-nodes pull-env)]
    (reduce (fn [acc [k v]]
              (assoc acc k (nodely.async/<demand v exception-ch)))
            {}
            scheduled-promises)))

(defn materialize-channel
  [^LazySchedulingEnvironment pull-env {::keys [exception-ch]}]
  (let [scheduled-promises (lazy-env/scheduled-nodes pull-env)]
    (nodely.async/jog
     (zipmap (keys scheduled-promises)
             (nodely.async/<request (async/map vector (vals scheduled-promises))
                                    exception-ch)))))

(defn eval
  ([env k]
   (eval env k {}))
  ([env k {::keys [max-sequence-parallelism
                   exception-ch]
           :or    {max-sequence-parallelism 4
                   exception-ch             (async/promise-chan)}}]
   (let [pull-env (pull-env env {::max-sequence-parallelism max-sequence-parallelism
                                 ::exception-ch             exception-ch})]
     (get pull-env k)
     (merge env (materialize pull-env {::exception-ch exception-ch})))))

(defn eval-key-channel
  ([env k]
   (eval-key-channel env k {}))
  ([env k {::keys [max-sequence-parallelism
                   exception-ch]
           :or    {max-sequence-parallelism 4
                   exception-ch             (async/promise-chan)}}]
   (async/go
     (let [pull-env (pull-env env {::max-sequence-parallelism max-sequence-parallelism
                                   ::exception-ch             exception-ch})]
       (get pull-env k)
       (let [[val port] (async/alts! [exception-ch (materialize-channel pull-env {::exception-ch exception-ch})]
                                     :priority true)]
         (if (= port exception-ch)
           val
           (data/get-value val k)))))))

(defn eval-key
  ([env k]
   (eval-key env k {}))
  ([env k {::keys [max-sequence-parallelism
                   exception-ch]
           :or    {max-sequence-parallelism 4
                   exception-ch             (async/promise-chan)}}]
   (let [pull-env (pull-env env {::max-sequence-parallelism max-sequence-parallelism
                                 ::exception-ch             exception-ch})]
     (get pull-env k)
     (data/get-value (materialize pull-env {::exception-ch exception-ch}) k))))

(defn eval-node
  [node env]
  (eval-key (assoc env ::target node) ::target))
