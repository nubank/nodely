(ns nodely.engine.manifold-test
  (:refer-clojure :exclude [eval async])
  (:require
   [clojure.test :refer :all]
   [criterium.core :refer [time-body]]
   [matcher-combinators.matchers :as matchers]
   [matcher-combinators.test :refer [match?]]
   [nodely.engine.core :as core]
   [nodely.engine.manifold :as manifold]
   [nodely.syntax :as syntax :refer [>leaf]]))

(def test-env {:a (>leaf (+ 1 2))
               :b (>leaf (* ?a 2))
               :c (>leaf (* ?a 3))
               :d (>leaf {:b ?b
                          :c ?c})})

(def test-env+delay {:a (>leaf (+ 1 2))
                     :b (>leaf (do (Thread/sleep 1000)
                                   (* ?a 2)))
                     :c (>leaf (do (Thread/sleep 1000)
                                   (* ?a 3)))
                     :d (>leaf {:a ?a
                                :b ?b
                                :c ?c})})

(def env-with-sequence {:a (>leaf [1 2 3])
                        :b (syntax/>sequence inc ?a)})

(def env-with-sequence-with-closure
  {:a (syntax/>value [1 2 3])
   :c (syntax/>value 2)
   :b (syntax/>sequence #(* % ?c) ?a)})

(def env-with-sequence+delay {:a (>leaf [1 2 3])
                              :b (syntax/>sequence
                                  #(do (Thread/sleep 1000) (inc %))
                                  ?a)})

(deftest eval-env
  (testing "async response is equal to sync response"
    (is (core/resolve :d test-env)
        (manifold/eval-env test-env)))
  (testing "async version takes half the time of sync version
            (runtime diff is 1 sec, within a tolerance of 10ms"
    (let [[nanosec-sync _]  (time-body (core/resolve :d test-env+delay))
          [nanosec-async _] (time-body (manifold/eval-env test-env+delay))]
      (is (match? (matchers/within-delta 10000000 1000000000)
                  (- nanosec-sync nanosec-async))))))

(deftest eval-env-with-sequence
  (testing "async response is equal to sync response"
    (is (core/resolve :b env-with-sequence)
        (manifold/eval-env env-with-sequence)))
  (testing "async version takes a third of the time of sync version
            (runtime diff is 2 sec, within a tolerance of 10ms"
    (let [[nanosec-sync _]  (time-body (core/resolve :b env-with-sequence+delay))
          [nanosec-async _] (time-body (manifold/eval-env env-with-sequence+delay))]
      (is (match? (matchers/within-delta 10000000 2000000000)
                  (- nanosec-sync nanosec-async)))))
  (testing "Actually computes the correct answers"
    (is (= [2 3 4] (manifold/eval-key env-with-sequence+delay :b))))
  (testing "Supports closure of nodes in the iterated fn"
    (is (= [2 4 6] (manifold/eval-key env-with-sequence-with-closure :b)))))

(deftest eval-test
  (testing "eval node async"
    (is (match? [9 6]
                (manifold/eval-node (syntax/>cond
                                     (>leaf (odd? ?c)) (>leaf [?c ?b])
                                     (>leaf :else) (>leaf :nothing-else-matters)) test-env)))))
