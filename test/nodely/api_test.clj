(ns nodely.api-test
  (:refer-clojure :exclude [cond])
  (:require
   [clojure.core.async :as async]
   [clojure.test :refer :all]
   [nodely.api.v0 :as api :refer [>leaf >value eval-key-channel eval-node]]))

(def env {:x (>value 2)
          :y (>value 3)
          :z (>leaf (+ ?x ?y))})

(def missing-node-env
  {:a (>value 2)
   :c (>leaf (+ ?a ?b))})

(deftest eval-key-channel-test
  (testing "returning a result to a channel with each engine"
    (doseq [engine (->> api/engine-data
                        (filter (fn [[k v]] (::eval-key-channel v)))
                        (map first))]
      (is (= 5 (async/<!! (eval-key-channel env :z
                                            {::api/engine engine})))))))

(deftest eval-node-missing-node-exception-test
  (testing "evaling an env where all referred nodes exist works"
    (doseq [engine (keys api/engine-data)]
      (is (= 5 (eval-node env :z
                          {::api/engine engine})))))
  (testing "evaling an env where a key is missing raises an exception that assists diagnosing the problematic environment"
    (doseq [engine (keys api/engine-data)]
      (is (thrown-with-msg? IllegalArgumentException #"missing node named :b"
                            (eval-node missing-node-env :c
                                       {::api/engine engine}))))))

#_(deftest nested-cond-macros
    (testing "using internal variables"
      (is (= 0
             (lazy/eval-node-with-values
              (>cond
               (>leaf (even? ?z)) (>leaf (+ 2 2))
               (>leaf (even? ?y)) (>cond
                                   (>leaf (= 42 ?y)) (>leaf (- 42 ?y))
                                   (>leaf (odd? ?z)) (>leaf :z-is-odd)))
              {:z 1, :y 42}))))
    (testing "using api variables"
      (is (= 0
             (nodely/eval-node-with-values
              (nodely/cond
                (even? ?z) (+ 2 2)
                (even? ?y) (>node (nodely/cond
                                    (= 42 ?y) (- 42 ?y)
                                    (odd? ?z) :z-is-odd)))
              {:z 1, :y 42}))))
    (testing "mixing internal and api variables"
      (is (= 0
             (nodely/eval-node-with-values
              (nodely/cond
                (even? ?z) (+ 2 2)
                (even? ?y) (>node (syntax/cond
                                    (= 42 ?y) (- 42 ?y)
                                    (odd? ?z) :z-is-odd)))
              {:z 1, :y 42})))))
