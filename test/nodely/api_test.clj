(ns nodely.api-test
  (:refer-clojure :exclude [cond])
  (:require
   [clojure.core.async :as async]
   [clojure.test :refer :all]
   [nodely.api.v0 :as api :refer [>leaf >value >sequence]]
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
                         (catch clojure.lang.ExceptionInfo e (ex-message e))))))))

(t/deftest new-eval-node-sequence
  (for [engine (keys api/engine-data)]
    (engine-test-suite engine)))
