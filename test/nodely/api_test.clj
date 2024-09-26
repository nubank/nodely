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

(def env+channel-leaf {:a (>value 1)
                       :b (api/>channel-leaf
                           (async/go (+ ?a 5)))
                       :c (>leaf (+ ?a ?b))})

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

(defn ensure-unrealized-delay
  [sym]
  (when (realized? (deref (resolve sym)))
    (require :reload '[nodely.api.v0])))

(defmacro with-fresh-delay
  [delay & body]
  `(do (ensure-unrealized-delay (quote ~delay))
       (let [res# ~@body]
         (require :reload 'nodely.api.v0)
         res#)))

(t/deftest engine-without-support
  (t/testing "attempting to use virtual futures without virtual futures in the JVM"
    (with-fresh-delay nodely.api.v0/virtual-future-failure
      (t/matching
       #"Classloader could not locate `java.util.concurrent.ThreadPerTaskExecutor`"
       (with-redefs [require (fn [& args]
                               (throw (ex-info "Kaboom! We're not on JVM 21 for pretend"
                                               {:cause :test-virtual-future-failure})))]
         (try (api/eval-key-channel env :z {::api/engine :applicative.virtual-future})
              (catch Throwable t
                (ex-message t)))))))
  (t/testing "attempting to use core.async without core.async on the classpath"
    (with-fresh-delay nodely.api.v0/core-async-failure
      (t/matching
       #"Could not locate core-async on classpath"
       (with-redefs [require (fn [& args]
                               (throw (ex-info "Kaboom! We don't have core.async for pretend"
                                               {:cause :test-core-async-failure})))]
         (try (api/eval-key-channel env :z {::api/engine :core-async.lazy-scheduling})
              (catch Throwable t
                (ex-message t)))))))
  (t/testing "attempting to use manifold without manifold on the classpath"
    (with-fresh-delay nodely.api.v0/manifold-failure
      (t/matching
       #"Could not locate manifold on classpath"
       (with-redefs [require (fn [& args]
                               (throw (ex-info "Kaboom! We're don't have manifold for pretend"
                                               {:cause :test-manifold-failure})))]
         (try (api/eval-key-channel env :z {::api/engine :async.manifold})
              (catch Throwable t
                (ex-message t)))))))
  (t/testing "attempting to use promesa without promesa on the classpath"
    (with-fresh-delay nodely.api.v0/promesa-failure
      (t/matching
       #"Could not locate promesa on classpath"
       (with-redefs [require (fn [& args]
                               (throw (ex-info "Kaboom! We're don't have promesa for pretend"
                                               {:cause :test-promesa-failure})))]
         (try (api/eval-key-channel env :z {::api/engine :applicative.promesa})
              (catch Throwable t
                (ex-message t))))))))

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
                       (catch clojure.lang.ExceptionInfo e (ex-message e)))))

    (t/testing "env with >channel-leaf"
      (t/matching 7 (api/eval-key env+channel-leaf :c {::api/engine engine-key})))))

(t/deftest api-test
  (for [engine (set/difference (set (keys api/engine-data))
                               #{:core-async.iterative-scheduling
                                 :async.virtual-futures})]
    (engine-test-suite engine)))

(t/deftest incorrect-engine-id
  (t/testing "we communicate how the client has specified an invalid input and what would be valid inputs"
    (t/testing "eval"
      (t/matching #"Unsupported engine specified, please specify a supported engine."
                  (try (api/eval env :z {::api/engine :core.async.doesnt-exist})
                       (catch clojure.lang.ExceptionInfo e (ex-message e))))
      (t/matching {:specified-engine-name  :core.async.doesnt-exist
                   :supported-engine-names set?}
                  (try (api/eval env :z {::api/engine :core.async.doesnt-exist})
                       (catch clojure.lang.ExceptionInfo e (ex-data e)))))
    (t/testing "eval-key"
      (t/matching #"Unsupported engine specified, please specify a supported engine."
                  (try (api/eval-key env :z {::api/engine :core.async.doesnt-exist})
                       (catch clojure.lang.ExceptionInfo e (ex-message e))))
      (t/matching {:specified-engine-name  :core.async.doesnt-exist
                   :supported-engine-names set?}
                  (try (api/eval-key env :z {::api/engine :core.async.doesnt-exist})
                       (catch clojure.lang.ExceptionInfo e (ex-data e)))))
    (t/testing "eval-key-channel"
      (t/matching #"Unsupported engine specified, please specify a supported engine."
                  (try (api/eval-key-channel env :z {::api/engine :core.async.doesnt-exist})
                       (catch clojure.lang.ExceptionInfo e (ex-message e))))
      (t/matching {:specified-engine-name  :core.async.doesnt-exist
                   :supported-engine-names set?}
                  (try (api/eval-key-channel env :z {::api/engine :core.async.doesnt-exist})
                       (catch clojure.lang.ExceptionInfo e (ex-data e)))))
    (t/testing "eval-node"
      (t/matching #"Unsupported engine specified, please specify a supported engine."
                  (try (api/eval-node env (>leaf (inc ?z)) {::api/engine :core.async.doesnt-exist})
                       (catch clojure.lang.ExceptionInfo e (ex-message e))))
      (t/matching {:specified-engine-name  :core.async.doesnt-exist
                   :supported-engine-names set?}
                  (try (api/eval-node env (>leaf (inc ?z)) {::api/engine :core.async.doesnt-exist})
                       (catch clojure.lang.ExceptionInfo e (ex-data e)))))
    (t/testing "eval-node-channel"
      (t/matching #"Unsupported engine specified, please specify a supported engine."
                  (try (api/eval-node-channel env (>leaf (inc ?z)) {::api/engine :core.async.doesnt-exist})
                       (catch clojure.lang.ExceptionInfo e (ex-message e))))
      (t/matching {:specified-engine-name  :core.async.doesnt-exist
                   :supported-engine-names set?}
                  (try (api/eval-node-channel env (>leaf (inc ?z)) {::api/engine :core.async.doesnt-exist})
                       (catch clojure.lang.ExceptionInfo e (ex-data e)))))))
