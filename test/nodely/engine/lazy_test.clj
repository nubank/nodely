(ns nodely.engine.lazy-test
  (:refer-clojure :exclude [eval resolve])
  (:require
   [clojure.test :refer :all]
   [matcher-combinators.test :refer [match?]]
   [nodely.data :as data]
   [nodely.engine.lazy :as lazy]))

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
