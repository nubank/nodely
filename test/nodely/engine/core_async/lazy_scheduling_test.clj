(ns nodely.engine.core-async.lazy-scheduling-test
  (:refer-clojure :exclude [eval async])
  (:require
   [clojure.core.async :as async]
   [clojure.test :refer :all]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.properties :as prop]
   [criterium.core :refer [time-body]]
   [matcher-combinators.matchers :as matchers]
   [matcher-combinators.test :refer [match? thrown-match?]]
   [nodely.data :as data]
   [nodely.engine.core :as core]
   [nodely.engine.core-async.core :as nodely.async]
   [nodely.engine.core-async.lazy-scheduling :as nasync]
   [nodely.engine.lazy :as engine.lazy]
   [nodely.fixtures :as fixtures]
   [nodely.syntax :as syntax :refer [>cond >leaf >sequence >value blocking]]))

(def test-env {:a (>leaf (+ 1 2))
               :b (>leaf (* ?a 2))
               :c (>leaf (* ?a 3))
               :d (>leaf {:b ?b
                          :c ?c})})

(def env-with-missing-key {:b (>leaf (* ?a 2))
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

(def env-with-sequence {:a (>leaf [1 2 3])
                        :b (>sequence inc ?a)})

(def env-with-closure-sequence {:a (>value [1 2 3])
                                :c (>value 2)
                                :b (>sequence (fn [e] (* e ?c)) ?a)})

(def env+sequence-with-nil-values
  {:a (>leaf [1 2 nil 4])
   :b (syntax/>sequence #(when % (inc %)) ?a)})

(def env+sequence-returning-nil-values
  {:a (>leaf [1 2 3 4])
   :b (syntax/>sequence #(if (even? %)
                           nil
                           %) ?a)})

(def env-with-sequence+delay {:a (>leaf [1 2 3])
                              :b (syntax/>sequence
                                  #(do (Thread/sleep 1000) (inc %))
                                  ?a)})

(def env+exception {:a (>value 1)
                    :b (>leaf (throw (ex-info "Oops" {:some-data "data"})))
                    :c (>leaf (+ ?a ?b))})

(def env+channel-leaf {:a (>value 1)
                       :b (nodely.async/>channel-leaf
                           (async/go (+ ?a 5)))
                       :c (>leaf (+ ?a ?b))})

(def env+channel-throw {:a (>value 1)
                        :b (nodely.async/>channel-leaf
                            (async/go (throw (ex-info "Exception in Channel"
                                                      {:data "data"}))))
                        :c (>leaf (+ ?a ?b))})

(def env-with-blocking-tag {:a (>leaf (Thread/currentThread))
                            :b (blocking (>leaf (Thread/currentThread)))
                            :a-name (>leaf (.getName ?a))
                            :b-name (>leaf (.getName ?b))
                            :c (>leaf (str ?a-name " " ?b-name))})

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

(deftest eval-env
  (testing "async response is equal to sync response"
    (is (core/resolve :d test-env)
        (nasync/eval-key test-env :d)))
  (testing "async response is equal to sync response with async user channels"
    (is (engine.lazy/eval-key env+channel-leaf :c)
        (nasync/eval-key env+channel-leaf :c)))
  (testing "async version takes half the time of sync version
            (runtime diff is 1 sec, within a tolerance of 3ms"
    (let [[nanosec-sync _]  (time-body (core/resolve :d test-env+delay))
          [nanosec-async _] (time-body (nasync/eval-key test-env+delay :d))]
      (is (match? (matchers/within-delta 100000000 1000000000)
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
    (is (match? [2 3 4] (nasync/eval-key env-with-sequence+delay :b))))
  (testing "When there's a closure in the sequence expr"
    (is (match? [2 4 6] (nasync/eval-key env-with-closure-sequence :b)))))

(deftest eval-test
  (testing "eval node async"
    (is (match? [9 6]
                (nasync/eval-node (>cond
                                   (>leaf (odd? ?c)) (>leaf [?c ?b])
                                   :else :nothing-else-matters) test-env))))
  (testing "eval with missing keys"
    (is (thrown-match? clojure.lang.ExceptionInfo
                       {:key :a}
                       (nasync/eval-key env-with-missing-key :c))))
  (testing "eval async with exception"
    (is (thrown-match? clojure.lang.ExceptionInfo
                       {:some-data "data"}
                       (nasync/eval-key env+exception :c))))
  (testing "eval node async for interesting example"
    (is (match? 14
                (nasync/eval-key interesting-example :z))))
  (testing "trivial example"
    (is (match? false
                (nasync/eval-key {:a (data/value false)} :a))))
  (testing "tricky example"
    (is (match? 4
                (nasync/eval-key tricky-example :z))))
  (testing "env with channel leaf"
    (is (match? 7
                (nasync/eval-key env+channel-leaf :c))))
  (testing "env with channel throwing exception"
    (is (thrown-match? clojure.lang.ExceptionInfo
                       {:channel any?}
                       (nasync/eval-key env+channel-throw :c)))))

(deftest eval-channel-test
  (testing "eval node async for interesting example"
    (is (match? 14
                (async/<!! (nasync/eval-key-channel interesting-example :z)))))
  (testing "trivial example"
    (is (match? false
                (async/<!! (nasync/eval-key-channel {:a (data/value false)} :a)))))
  (testing "tricky example"
    (is (match? 4
                (async/<!! (nasync/eval-key-channel tricky-example :z)))))
  (testing "env with exception"
    (is (match? {:some-data "data"}
                (ex-data (async/<!! (nasync/eval-key-channel env+exception :c))))))
  (testing "env with channel leaf throwing"
    (is (match?
         {:channel any?}
         (ex-data (async/<!! (nasync/eval-key-channel env+channel-throw :c)))))))

(defspec does-not-blow-up-spec
  (prop/for-all [env (fixtures/env-gen {})]
                (nasync/eval-key env (rand-nth (keys env)))
                true))

(deftest blocking-eval-test
  (testing "eval of a blocking tagged node will happen in the `async-thread-macro` worker pool"
    (is (match? #"async-thread-macro-\d+"
                (nasync/eval-key env-with-blocking-tag :b-name)))
    (is (match? #"async-dispatch-\d+"
                (nasync/eval-key env-with-blocking-tag :a-name)))))

(deftest thread-sleeping-test-proves-thread-works-how-we-expect
  (testing "actually only 8 threads in the async dispatch worker pool"
    (is (match? 8
                @@#'clojure.core.async.impl.exec.threadpool/pool-size)))
  (testing "when we have 9 1-second blocking nodes in one environment, it can run in fewer than 2 seconds"
    (testing "async version runs parallel when option is neglected"
      (let [[nanosec-async _] (time-body (nasync/eval-key env-with-nine-sleeps :z))]
        (is (match? (matchers/within-delta 1000000000 1000000000)
                    nanosec-async))))))
