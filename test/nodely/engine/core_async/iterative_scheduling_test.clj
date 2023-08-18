(ns nodely.engine.core-async.iterative-scheduling-test
  (:refer-clojure :exclude [eval async])
  (:require
   [clojure.test :refer :all]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.properties :as prop]
   [criterium.core :refer [time-body]]
   [matcher-combinators.matchers :as matchers]
   [matcher-combinators.test :refer [match?]]
   [nodely.data :as data]
   [nodely.engine.core :as core]
   [nodely.engine.core-async.iterative-scheduling :as nasync]
   [nodely.fixtures :as fixtures]
   [nodely.syntax :as syntax :refer [>cond >leaf]]))

(def test-env {:a (>leaf (+ 1 2))
               :b (>leaf (* ?a 2))
               :c (>leaf (* ?a 3))
               :d (>leaf {:b ?b
                          :c ?c})})

(def interesting-example {:x (data/value 22)
                          :y (data/value 13)
                          :w (data/value 1)
                          :z (data/branch (>leaf (even? ?w))
                                          (>leaf (inc ?x))
                                          (>leaf (inc ?y)))})

(def tricky-example {:x (data/value 1)
                     :y (data/value 2)
                     :a (data/value 3)
                     :b (data/value 4)
                     :c (data/value 5)
                     :w (data/branch (data/branch
                                      (>leaf (even? ?x))
                                      (>leaf ?y)
                                      (>leaf ?a))
                                     (>leaf ?b)
                                     (>leaf ?c))
                     :z (>leaf ?w)})

(def test-env+delay {:a (>leaf (+ 1 2))
                     :b (>leaf (do (Thread/sleep 1000)
                                   (* ?a 2)))
                     :c (>leaf (do (Thread/sleep 1000)
                                   (* ?a 3)))
                     :d (>leaf {:a ?a
                                :b ?b
                                :c ?c})})

(def env+sequence-with-nil-values
  {:a (>leaf [1 2 nil 4])
   :b (syntax/>sequence #(when % (inc %)) ?a)})

(def env+sequence-returning-nil-values
  {:a (>leaf [1 2 3 4])
   :b (syntax/>sequence #(if (even? %)
                           nil
                           %) ?a)})

(def env-with-sequence {:a (>leaf [1 2 3])
                        :b (syntax/>sequence inc ?a)})

(def env-with-sequence+delay {:a (>leaf [1 2 3])
                              :b (syntax/>sequence
                                  #(do (Thread/sleep 1000) (inc %))
                                  ?a)})

(deftest eval-env
  (testing "async response is equal to sync response"
    (is (core/resolve :d test-env)
        (nasync/eval-key test-env :d)))
  (testing "async version takes half the time of sync version
            (runtime diff is 1 sec, within a tolerance of 3ms"
    (let [[nanosec-sync _]  (time-body (core/resolve :d test-env+delay))
          [nanosec-async _] (time-body (nasync/eval-key test-env+delay :d))]
      (is (match? (matchers/within-delta 6000000 1000000000)
                  (- nanosec-sync nanosec-async))))))

(deftest eval-env-with-sequence
  (testing "async response is equal to sync response"
    (is (core/resolve :b env-with-sequence)
        (nasync/eval-key env-with-sequence :b {::nasync/max-sequence-parallelism 4})))
  (testing "sync=async for sequence with nil values"
    (is (core/resolve :b env+sequence-with-nil-values)
        (nasync/eval-key env+sequence-with-nil-values :b)))
  (testing "sync=async for sequence returning nil values"
    (is (core/resolve :b env+sequence-returning-nil-values)
        (nasync/eval-key env+sequence-returning-nil-values :b)))
  (testing "async version takes a third of the time of sync version
            (runtime diff is 2 sec, within a tolerance of 3ms"
    (let [[nanosec-sync _]  (time-body (core/resolve :b env-with-sequence+delay))
          [nanosec-async _] (time-body (nasync/eval-key env-with-sequence+delay :b {::nasync/max-sequence-parallelism 4}))]
      (is (match? (matchers/within-delta 8000000 2000000000)
                  (- nanosec-sync nanosec-async)))))
  (testing "async version runs parallel when option is neglected"
    (let [[nanosec-sync _]  (time-body (core/resolve :b env-with-sequence+delay))
          [nanosec-async _] (time-body (nasync/eval-key env-with-sequence+delay :b {}))]
      (is (match? (matchers/within-delta 8000000 2000000000)
                  (- nanosec-sync nanosec-async)))))
  (testing "Actually computes the correct answers"
    (is (= [2 3 4] (nasync/eval-key env-with-sequence+delay :b)))))

(deftest eval-test
  (testing "eval node async"
    (is (match? [9 6]
                (nasync/eval-node (>cond
                                   (>leaf (odd? ?c)) (>leaf [?c ?b])
                                   :else :nothing-else-matters) test-env))))
  (testing "eval node async for interesting example"
    (is (match? 14
                (nasync/eval-key interesting-example :z))))
  (testing "trivial example"
    (is (match? false
                (nasync/eval-key {:a (data/value false)} :a))))
  (testing "tricky example"
    (is (match? 4
                (nasync/eval-key tricky-example :z)))))

(defspec does-not-blow-up-spec
  (prop/for-all [env (fixtures/env-gen {:max-branch-count 20})]
                (nasync/eval-key env (rand-nth (keys env)))
                true))
