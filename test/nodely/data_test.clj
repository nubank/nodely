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
  (testing "branch node - execution path only"
    (is (= #{:a} (data/node-inputs
                  (data/branch (data/leaf [:a] identity)
                               (data/leaf [:b] identity) ; truthy - not included
                               (data/leaf [:c] identity)))))) ; falsey - not included
  (testing "sequence node"
    (is (= #{:a} (data/node-inputs (data/sequence :a identity)))))
  (testing "nested branches - follows condition path"
    (is (= #{:x} (data/node-inputs (data/branch (data/branch (data/leaf [:x] identity)
                                                             (data/leaf [:y] identity)
                                                             (data/leaf [:a] identity))
                                                (data/leaf [:b] identity)
                                                (data/leaf [:c] identity)))))))

(deftest node-all-inputs
  (testing "value node"
    (is (= #{} (data/node-all-inputs (data/value 42)))))
  (testing "leaf node"
    (is (= #{:a} (data/node-all-inputs (data/leaf [:a] identity)))))
  (testing "branch node - all paths"
    (is (= #{:a :b :c} (data/node-all-inputs
                        (data/branch (data/leaf [:a] identity) ; condition
                                     (data/leaf [:b] identity) ; truthy - included
                                     (data/leaf [:c] identity)))))) ; falsey - included
  (testing "sequence node"
    (is (= #{:a} (data/node-all-inputs (data/sequence :a identity)))))
  (testing "complex branch with nested dependencies"
    (let [condition-leaf (data/leaf [:age :threshold] identity)
          truthy-leaf (data/leaf [:name] identity)
          falsey-leaf (data/leaf [:guardian] identity)
          branch-node (data/branch condition-leaf truthy-leaf falsey-leaf)]
      ;; Should collect ALL possible dependencies
      (is (= #{:age :threshold :name :guardian}
             (data/node-all-inputs branch-node))))))

(deftest value?
  (testing "value node"
    (is (true? (data/value? (data/value 42)))))
  (testing "leaf node"
    (is (false? (data/value? (data/leaf [:a] identity)))))
  (testing "branch node"
    (is (false? (data/value? (data/branch (data/value 1) (data/value 2) (data/value 3))))))
  (testing "sequence node"
    (is (false? (data/value? (data/sequence :a identity))))))

(deftest leaf?
  (testing "value node"
    (is (false? (data/leaf? (data/value 42)))))
  (testing "leaf node"
    (is (true? (data/leaf? (data/leaf [:a] identity)))))
  (testing "branch node"
    (is (false? (data/leaf? (data/branch (data/value 1) (data/value 2) (data/value 3))))))
  (testing "sequence node"
    (is (false? (data/leaf? (data/sequence :a identity))))))

(deftest branch?
  (testing "value node"
    (is (false? (data/branch? (data/value 42)))))
  (testing "leaf node"
    (is (false? (data/branch? (data/leaf [:a] identity)))))
  (testing "branch node"
    (is (true? (data/branch? (data/branch (data/value 1) (data/value 2) (data/value 3))))))
  (testing "sequence node"
    (is (false? (data/branch? (data/sequence :a identity))))))

(deftest sequence?
  (testing "value node"
    (is (false? (data/sequence? (data/value 42)))))
  (testing "leaf node"
    (is (false? (data/sequence? (data/leaf [:a] identity)))))
  (testing "branch node"
    (is (false? (data/sequence? (data/branch (data/value 1) (data/value 2) (data/value 3))))))
  (testing "sequence node"
    (is (true? (data/sequence? (data/sequence :a identity))))))

(deftest node-type
  (testing "value node"
    (is (= :value (data/node-type (data/value 42)))))
  (testing "leaf node"
    (is (= :leaf (data/node-type (data/leaf [:a] identity)))))
  (testing "branch node"
    (is (= :branch (data/node-type (data/branch (data/value 1) (data/value 2) (data/value 3))))))
  (testing "sequence node"
    (is (= :sequence (data/node-type (data/sequence :a identity))))))

(deftest node-value
  (testing "value node"
    (is (= 42 (data/node-value (data/value 42))))
    (is (= :keyword (data/node-value (data/value :keyword))))
    (is (= "string" (data/node-value (data/value "string")))))
  (testing "non-value node returns nil"
    (is (nil? (data/node-value (data/leaf [:a] identity))))
    (is (nil? (data/node-value (data/branch (data/value 1) (data/value 2) (data/value 3)))))
    (is (nil? (data/node-value (data/sequence :a identity))))))

(deftest branch-condition
  (testing "branch node"
    (let [condition (data/value :condition)
          truthy (data/value :truthy)
          falsey (data/value :falsey)
          branch-node (data/branch condition truthy falsey)]
      (is (= condition (data/branch-condition branch-node)))))
  (testing "non-branch node returns nil"
    (is (nil? (data/branch-condition (data/value 42))))
    (is (nil? (data/branch-condition (data/leaf [:a] identity))))
    (is (nil? (data/branch-condition (data/sequence :a identity))))))

(deftest branch-truthy
  (testing "branch node"
    (let [condition (data/value :condition)
          truthy (data/value :truthy)
          falsey (data/value :falsey)
          branch-node (data/branch condition truthy falsey)]
      (is (= truthy (data/branch-truthy branch-node)))))
  (testing "non-branch node returns nil"
    (is (nil? (data/branch-truthy (data/value 42))))
    (is (nil? (data/branch-truthy (data/leaf [:a] identity))))
    (is (nil? (data/branch-truthy (data/sequence :a identity))))))

(deftest branch-falsey
  (testing "branch node"
    (let [condition (data/value :condition)
          truthy (data/value :truthy)
          falsey (data/value :falsey)
          branch-node (data/branch condition truthy falsey)]
      (is (= falsey (data/branch-falsey branch-node)))))
  (testing "non-branch node returns nil"
    (is (nil? (data/branch-falsey (data/value 42))))
    (is (nil? (data/branch-falsey (data/leaf [:a] identity))))
    (is (nil? (data/branch-falsey (data/sequence :a identity))))))

(deftest sequence-input
  (testing "sequence node"
    (let [seq-node (data/sequence :input-key identity)]
      (is (= :input-key (data/sequence-input seq-node)))))
  (testing "non-sequence node returns nil"
    (is (nil? (data/sequence-input (data/value 42))))
    (is (nil? (data/sequence-input (data/leaf [:a] identity))))
    (is (nil? (data/sequence-input (data/branch (data/value 1) (data/value 2) (data/value 3)))))))
