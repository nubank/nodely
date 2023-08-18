(ns nodely.engine.applicative-test
  (:require
   [clojure.test :refer :all]
   [criterium.core :as criterium]
   [matcher-combinators.matchers :as matchers]
   [matcher-combinators.test :refer [match?]]
   [nodely.data :as data]
   [nodely.engine.applicative :as applicative]
   [nodely.engine.core :as core]
   [nodely.syntax :as syntax :refer [>leaf >value]]
   [promesa.core :as p]))

(def test-env {:a (>value 2)
               :b (>value 1)
               :c (>leaf (+ ?a ?b))})

(def test-env+delay {:a (>leaf (+ 1 2))
                     :b (>leaf (p/delay 1000 (* ?a 2)))
                     :c (>leaf (p/delay 1000 (* ?a 3)))
                     :d (>leaf {:a ?a
                                :b ?b
                                :c ?c})})

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

(def env-with-sequence {:a (>leaf [1 2 3])
                        :b (syntax/>sequence inc ?a)})

(def env+sequence-with-nil-values
  {:a (>leaf [1 2 nil 4])
   :b (syntax/>sequence #(when % (inc %)) ?a)})

(def env+sequence-returning-nil-values
  {:a (>leaf [1 2 3 4])
   :b (syntax/>sequence #(if (even? %)
                           nil
                           %) ?a)})

(def env-with-sequence+delay {:a (>leaf [1 2 3])
                              :b (syntax/>sequence #(p/delay 1000 (inc %)) ?a)
                              :c (>leaf (mapv deref ?b))})

(def env-with-sequence+delay-sync {:a (>leaf [1 2 3])
                                   :b (syntax/>sequence #(do (Thread/sleep 1000) (inc %)) ?a)
                                   :c (>leaf ?b)})

(deftest eval-key-test
  (testing "eval promise"
    (is (match? 3 (applicative/eval-key test-env :c))))
  (testing "async works"
    (let [[time-ns result] (criterium/time-body (applicative/eval-key test-env+delay :d))]
      (is (match? {:a 3 :b 6 :c 9} result))
      (is (match? (matchers/within-delta 100000000 1000000000) time-ns))))
  (testing "tricky example"
    (is (match? 4 (applicative/eval-key tricky-example :z)))))

(deftest eval-test
  (testing "eval promise"
    (is (match? {:a {::data/value 2}
                 :b {::data/value 1}
                 :c {::data/value 3}}
                (applicative/eval test-env :c))))
  (testing "tricky example"
    (is (match? {:x (data/value 1)
                 :y (data/value 2)
                 :a (data/value 3)
                 :b (data/value 4)
                 :c (data/value 5)
                 :w (data/value 4)
                 :z {::data/type :leaf
                     ::data/inputs #{:w}}}
                (applicative/eval tricky-example :w)))))

(deftest eval-env-with-sequence
  (testing "async response is equal to sync response"
    (is (match? (-> (core/resolve :b env-with-sequence) (get :b) ::data/value)
                (applicative/eval-key env-with-sequence :b))))
  (testing "sync=async for sequence with nil values"
    (is (match? (-> (core/resolve :b env+sequence-with-nil-values) (get :b) ::data/value)
                (applicative/eval-key env+sequence-with-nil-values :b))))
  (testing "sync=async for sequence returning nil values"
    (is (match? (-> (core/resolve :b env+sequence-returning-nil-values) (get :b) ::data/value)
                (applicative/eval-key env+sequence-returning-nil-values :b))))
  (testing "async version takes a third of the time of sync version
            (runtime diff is 2 sec, within a tolerance of 3ms"
    (let [[nanosec-sync _]  (criterium/time-body (core/resolve :c env-with-sequence+delay-sync))
          [nanosec-async _] (criterium/time-body (applicative/eval-key env-with-sequence+delay :c))]
      (is (match? (matchers/within-delta 8000000 2000000000)
                  (- nanosec-sync nanosec-async)))))
  (testing "Actually computes the correct answers"
    (is (match? [2 3 4] (applicative/eval-key env-with-sequence+delay :c)))))
