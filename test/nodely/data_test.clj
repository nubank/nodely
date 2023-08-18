(ns nodely.data-test
  (:refer-clojure :exclude [cond])
  (:require
   [clojure.test :refer :all]
   [nodely.data :as data]))

(deftest node?
  (testing "an actual node"
    (is (true? (data/node? (data/value 42)))))
  (testing "not a node"
    (is (false? (data/node? 42)))))

(deftest node-inputs
  (testing "value node"
    (is (= #{} (data/node-inputs (data/value 42)))))
  (testing "leaf node"
    (is (= #{:a} (data/node-inputs (data/leaf [:a] identity)))))
  (testing "branch node"
    (is (= #{:a} (data/node-inputs
                  (data/branch (data/leaf [:a] identity) (data/value 42) (data/value 42))))))
  (testing "sequence node"
    (is (= #{:a} (data/node-inputs (data/sequence :a identity)))))
  (testing "nested branches"
    (is (= #{:x} (data/node-inputs (data/branch (data/branch (data/leaf [:x] identity)
                                                             (data/leaf [:y] identity)
                                                             (data/leaf [:a] identity))
                                                (data/leaf [:b] identity)
                                                (data/leaf [:c] identity)))))))
