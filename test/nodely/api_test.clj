(ns nodely.api-test
  (:refer-clojure :exclude [cond])
  (:require
   [clojure.core.async :as async]
   [clojure.set :as set]
   [clojure.test :refer :all]
   [criterium.core :as criterium]
   [matcher-combinators.matchers :as matchers]
   [nodely.api.v0 :as api :refer [>leaf >sequence >value blocking]]
   [nodely.test-helpers :as t]))

(def env {:x (>value 2)
          :y (>value 3)
          :z (>leaf (+ ?x ?y))})

(def missing-node-env
  {:a (>value 2)
   :c (>leaf (+ ?a ?b))})

(def sequence-node-env
  {:x (>value [1 2 3])
   :y (>sequence inc ?x)})

(def sequence-node-env-with-missing-key
  {:y (>sequence inc ?x)})

(def exceptions-all-the-way-down
  {:a (>leaf (throw (ex-info "Oops!" {})))
   :b (>leaf (inc ?a))
   :c (>leaf (inc ?b))
   :d (>leaf (inc ?c))})

(def env-with-nine-sleeps {:a (blocking (>leaf (do (Thread/sleep 1000) :a)))
                           :b (blocking (>leaf (do (Thread/sleep 1000) :b)))
                           :c (blocking (>leaf (do (Thread/sleep 1000) :c)))
                           :d (blocking (>leaf (do (Thread/sleep 1000) :d)))
                           :e (blocking (>leaf (do (Thread/sleep 1000) :e)))
                           :f (blocking (>leaf (do (Thread/sleep 1000) :f)))
                           :g (blocking (>leaf (do (Thread/sleep 1000) :g)))
                           :h (blocking (>leaf (do (Thread/sleep 1000) :h)))
                           :i (blocking (>leaf (do (Thread/sleep 1000) :i)))
                           :z (>leaf (into #{} [?a ?b ?c ?d ?e ?f ?g ?h ?i]))})

(def parallel-engines
  #{:core-async.lazy-scheduling
    :core-async.iterative-scheduling
    :async.manifold
    :applicative.promesa
    :applicative.core-async
    :applicative.virtual-future
    :async.virtual-futures})

(defn channel-interface
  [engine-key]
  (get-in api/engine-data [engine-key ::api/eval-key-channel]))

(defn engine-test-suite
  [engine-key]
  (t/testing (name engine-key)

    (when (channel-interface engine-key)
      (t/testing "eval-*-channel-test"
        (t/testing "returning a result to a channel with each engine"
          (t/matching 5 (async/<!! (api/eval-key-channel env :z {::api/engine engine-key}))))
        (t/testing "returning a result to a channel with each engine"
          (t/matching 5 (async/<!! (api/eval-node-channel env (>leaf ?z) {::api/engine engine-key}))))))

    (when (parallel-engines engine-key)
      (t/testing "when we have 9 1-second blocking nodes in one environment, it can run in fewer than 2 seconds"
        (t/testing "async version runs parallel when option is neglected"
          (let [[nanosec-async _] (criterium/time-body (api/eval-key env-with-nine-sleeps :z))]
            (t/matching (matchers/within-delta 1000000000 1000000000)
                        nanosec-async)))))

    (t/testing "eval-works-across-all-engines"
      (t/testing "evaling an env where all referred nodes exist works"
        (t/matching 5 (api/eval-node env (>leaf ?z) {::api/engine engine-key})))
      (t/testing "eval-key an env where all referred nodes exist works"
        (t/matching 5 (api/eval-key env :z {::api/engine engine-key}))))

    (t/testing "eval-node-missing-node-exception-test"
      (t/testing "evaling an env where a key is missing raises an exception that assists diagnosing the problematic environment"
        (t/matching #"Missing key on env" (try (api/eval-key missing-node-env :c {::api/engine engine-key})
                                               (catch clojure.lang.ExceptionInfo e (ex-message e))))))

    (t/testing "eval-node-sequence"
      (t/testing "not missing sequence"
        (t/matching [2 3 4] (api/eval-key sequence-node-env :y {::api/engine engine-key})))
      (t/testing "sequence with missing key"
        (t/matching #"Missing key on env" (try (api/eval-key sequence-node-env-with-missing-key :y {::api/engine engine-key})
                                               (catch clojure.lang.ExceptionInfo e (ex-message e))))))

    (t/testing "eval node sequence"
      (t/testing "not missing sequence"
        (t/matching [2 3 4]
                    (api/eval-key sequence-node-env :y {::api/engine engine-key})))
      (t/testing "sequence with missing key"
        (t/matching #"Missing key on env"
                    (try (api/eval-key sequence-node-env-with-missing-key :y {::api/engine engine-key})
                         (catch clojure.lang.ExceptionInfo e (ex-message e))))))

    (t/testing "handling nested exceptions"
      (t/matching #"Oops!"
                  (try (api/eval-key exceptions-all-the-way-down :d {::api/engine engine-key})
                       (catch clojure.lang.ExceptionInfo e (ex-message e)))))))

(t/deftest api-test
  (for [engine (set/difference (set (keys api/engine-data))
                               #{:core-async.iterative-scheduling
                                 :async.virtual-futures})]
    (engine-test-suite engine)))
