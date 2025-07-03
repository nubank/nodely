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

(deftest update-leaf-test
  (testing "update-leaf modifies the function by composing with the given function"
    (let [original-leaf (data/leaf [:x] (comp inc :x))
          updated-leaf (data/update-leaf original-leaf (partial * 2))
          expected-props {::data/type   :leaf
                          ::data/inputs #{:x}}]
      ; update-leaf should preserve other leaf properties
      (is (= expected-props (select-keys updated-leaf (keys expected-props))))
      ; update-leaf should modify the function by composing with the given function
      (is (= 12 ((::data/fn updated-leaf) {:x 5}))))))

(deftest update-branch-test
  (testing "update-branch without apply-to-condition updates truthy and falsey branches"
    (let [condition (data/leaf [:x] (comp even? :x))
          truthy (data/leaf [:y] (comp inc :y))
          falsey (data/leaf [:z] (comp dec :z))
          branch (data/branch condition truthy falsey)
          updated-branch (data/update-branch branch (partial * 2) {})]
      (is (= true ((::data/fn (::data/condition updated-branch)) {:x 2}))) ; (even? 2) = true
      (is (= 20 ((::data/fn (::data/truthy updated-branch)) {:y 9}))) ; (* 2 (inc 9)) = 20
      (is (= 18 ((::data/fn (::data/falsey updated-branch)) {:z 10}))))) ; (* 2 (dec 10)) = 18

  (testing "update-branch with apply-to-condition updates all branches including condition"
    (let [condition (data/leaf [:x] (comp inc :x))
          truthy (data/leaf [:y] (comp inc :y))
          falsey (data/leaf [:z] (comp inc :z))
          branch (data/branch condition truthy falsey)
          updated-branch (data/update-branch branch (partial * 2) {:apply-to-condition? true})] 
      (is (= 12 ((::data/fn (::data/condition updated-branch)) {:x 5}))) ; (* 2 (inc 5)) = 12
      (is (= 12 ((::data/fn (::data/truthy updated-branch)) {:y 5}))) ; (* 2 (inc 5)) = 12
      (is (= 12 ((::data/fn (::data/falsey updated-branch)) {:z 5}))))) ; (* 2 (inc 5)) = 12

  (testing "update-branch with nested branches updates recursively"
    (let [inner-condition (data/leaf [:a] (comp even? :a))
          inner-truthy (data/leaf [:b] (comp inc :b))
          inner-falsey (data/leaf [:c] (comp dec :c))
          inner-branch (data/branch inner-condition inner-truthy inner-falsey)
          outer-condition (data/leaf [:x] (comp even? :x))
          outer-truthy inner-branch
          outer-falsey (data/leaf [:z] (comp dec :z))
          outer-branch (data/branch outer-condition outer-truthy outer-falsey)
          updated-branch (data/update-branch outer-branch (partial * 2) {})]
      ;; Outer condition should not be updated by default
      (is (= true ((::data/fn (::data/condition updated-branch)) {:x 2}))) ; (even? 2) = true
      ;; Inner condition should not be updated by default
      (is (= true ((::data/fn (::data/condition (::data/truthy updated-branch))) {:a 2}))) ; (even? 2) = true
      ;; Inner truthy should be updated
      (is (= 12 ((::data/fn (::data/truthy (::data/truthy updated-branch))) {:b 5}))) ; (* 2 (inc 5)) = 12
      ;; Inner falsey should be updated
      (is (= 8 ((::data/fn (::data/falsey (::data/truthy updated-branch))) {:c 5}))) ; (* 2 (dec 5)) = 8
      ;; Outer falsey should be updated
      (is (= 8 ((::data/fn (::data/falsey updated-branch)) {:z 5})))))) ; (* 2 (dec 5)) = 8

(deftest update-sequence-test
  (testing "update-sequence composes the function argument"
    (let [sequence-node (data/sequence :items [:closure-var] (comp inc :closure-var) #{})
          updated-sequence (data/update-sequence sequence-node (partial * 2))]
      (is (= 12 ((::data/fn (::data/process-node updated-sequence)) {:closure-var 5})))))) ; (* 2 (inc 5)) = 12

(deftest update-node-test
  (testing "update-node with value node"
    (let [value-node (data/value 42)
          updated-node (data/update-node value-node (partial * 2))]
      (is (= (::data/type updated-node) :value))
      (is (= 84 (::data/value updated-node))))) ; (* 2 42) = 84

  (testing "update-node with leaf node"
    (let [leaf-node (data/leaf [:x] (comp inc :x))
          updated-node (data/update-node leaf-node (partial * 2))]
      (is (= (::data/type updated-node) :leaf))
      (is (= (::data/inputs updated-node) #{:x}))
      (is (= 12 ((::data/fn updated-node) {:x 5}))))) ; (* 2 (inc 5)) = 12

  (testing "update-node with branch node"
    (let [condition (data/leaf [:x] (comp even? :x))
          truthy (data/value 10)
          falsey (data/value 20)
          branch-node (data/branch condition truthy falsey)
          updated-node (data/update-node branch-node (partial * 2))]
      (is (= (::data/type updated-node) :branch))
      ;; Condition should not be updated by default
      (is (= (::data/condition updated-node) condition))
      ;; Truthy and falsey should be updated
      (is (= 20 (::data/value (::data/truthy updated-node)))) ; (* 2 10) = 20
      (is (= 40 (::data/value (::data/falsey updated-node)))))) ; (* 2 20) = 40

  (testing "update-node with branch node and apply-to-condition option"
    (let [condition (data/leaf [:x] (comp inc :x))
          truthy (data/value 10)
          falsey (data/value 20)
          branch-node (data/branch condition truthy falsey)
          updated-node (data/update-node branch-node (partial * 2) {:apply-to-condition? true})]
      (is (= (::data/type updated-node) :branch))
      ;; All parts should be updated
      (is (= 12 ((::data/fn (::data/condition updated-node)) {:x 5}))) ; (* 2 (inc 5)) = 12
      (is (= 20 (::data/value (::data/truthy updated-node)))) ; (* 2 10) = 20  
      (is (= 40 (::data/value (::data/falsey updated-node)))))) ; (* 2 20) = 40

  (testing "update-node with sequence node"
    (let [sequence-node (data/sequence :items 5)
          updated-node (data/update-node sequence-node (partial * 2))]
      (is (= (::data/type updated-node) :sequence))
      (is (= (::data/input updated-node) :items))
      (is (= 10 (::data/value (::data/process-node updated-node)))))) ; (* 2 5) = 10

  (testing "update-node with 2-arity (no options)"
    (let [leaf-node (data/leaf [:x] (comp inc :x))
          updated-node (data/update-node leaf-node (partial * 2))]
      (is (= (::data/type updated-node) :leaf))
      (is (= (::data/inputs updated-node) #{:x}))
      (is (= 12 ((::data/fn updated-node) {:x 5}))))))