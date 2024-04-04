(ns nodely.api-test
  (:refer-clojure :exclude [cond])
  (:require
   [clojure.core.async :as async]
   [clojure.test :refer :all]
   [nodely.api.v0 :as api :refer [>leaf >value]]))

(def env {:x (>value 2)
          :y (>value 3)
          :z (>leaf (+ ?x ?y))})

(def missing-node-env
  {:a (>value 2)
   :c (>leaf (+ ?a ?b))})

(deftest eval-*-channel-test
  (testing "returning a result to a channel with each engine"
    (doseq [engine (->> api/engine-data
                        (filter (fn [[k v]] (::api/eval-key-channel v)))
                        (map first))]
      (is (= 5 (async/<!! (api/eval-key-channel env :z
                                                {::api/engine engine}))))))
  (testing "returning a result to a channel with each engine"
    (doseq [engine (->> api/engine-data
                        (filter (fn [[k v]] (::api/eval-key-channel v)))
                        (map first))]
      (is (= 5 (async/<!! (api/eval-node-channel env (>leaf ?z)
                                                 {::api/engine engine})))))))

(deftest eval-works-across-all-engines
  (testing "evaling an env where all referred nodes exist works"
    (doseq [engine (keys api/engine-data)]
      (is (= 5 (api/eval-node env (>leaf ?z)
                              {::api/engine engine})))))
  (testing "eval-key an env where all referred nodes exist works"
    (doseq [engine (keys api/engine-data)]
      (is (= 5 (api/eval-key env :z
                             {::api/engine engine}))))))

(deftest eval-node-missing-node-exception-test
  (testing "evaling an env where a key is missing raises an exception that assists diagnosing the problematic environment"
    (doseq [engine (keys api/engine-data)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo  #"Missing key on env"
                            (api/eval-key missing-node-env :c
                                          {::api/engine engine}))))))
