(ns nodely.engine.lazy-test
  (:refer-clojure :exclude [eval resolve])
  (:require
   [clojure.core.async :as async]
   [clojure.test :refer :all]
   [matcher-combinators.test :refer [match?]]
   [nodely.data :as data]
   [nodely.engine.lazy :as lazy]
   [nodely.syntax :refer [>leaf]]))

(def env-with-missing-key {:b (>leaf (* ?a 2))
                           :c (>leaf (* ?a 3))
                           :d (>leaf {:b ?b
                                      :c ?c})})

(deftest eval-node
  (testing "eval"
    (is (match? 5
                (lazy/eval-node (data/branch (data/leaf [:x] (fn [{:keys [x]}] (odd? x)))
                                             (data/value :odd)
                                             (data/leaf [:y :x]
                                                        (fn [{:keys [x y]}]
                                                          (+ x y))))
                                {:x (data/value 2)
                                 :y (data/value 3)})))))

(deftest eval-node-with-values
  (testing "eval"
    (is (match? 5
                (lazy/eval-node-with-values (data/branch (data/leaf [:x] (fn [{:keys [x]}] (odd? x)))
                                                         (data/value :odd)
                                                         (data/leaf [:y :x]
                                                                    (fn [{:keys [x y]}]
                                                                      (+ x y))))
                                            {:x 2
                                             :y 3})))))

(deftest missing-key-test
  (testing "missing key helpful error message"
    (is (thrown-match? clojure.lang.ExceptionInfo
                       {:key :a}
                       (lazy/eval-key env-with-missing-key :c)))))

(def eval-key-channel-env {:x (data/value 2)
                           :y (data/value 3)
                           :z (data/branch (data/leaf [:x] (fn [{:keys [x]}] (odd? x)))
                                           (data/value :odd)
                                           (data/leaf [:y :x]
                                                      (fn [{:keys [x y]}]
                                                        (+ x y))))})

(deftest eval-key-channel
  (testing "eval and getting a channel back"
    (is (match? 5
                (async/<!!
                 (lazy/eval-key-channel eval-key-channel-env :z))))))