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

(deftest with-error-handler-test
  (testing "with-error-handler wraps leaf functions to handle thrown exceptions"
    (let [throw-env {:a (data/leaf #{} (fn [] (throw (ex-info "OOps" {}))))}
          handled-env (data/with-error-handler throw-env (fn [^Throwable _] :handled))]
      (is (= :handled ((::data/fn (get handled-env :a))))))))

{:a (data/leaf #{} (fn [] (throw (ex-info "OOps" {}))))}
