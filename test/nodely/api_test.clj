(ns nodely.api-test
  (:refer-clojure :exclude [cond])
  (:require [clojure.test :refer :all]))

#_(deftest nested-cond-macros
    (testing "using internal variables"
      (is (= 0
             (lazy/eval-node-with-values
              (>cond
               (>leaf (even? ?z)) (>leaf (+ 2 2))
               (>leaf (even? ?y)) (>cond
                                   (>leaf (= 42 ?y)) (>leaf (- 42 ?y))
                                   (>leaf (odd? ?z)) (>leaf :z-is-odd)))
              {:z 1, :y 42}))))
    (testing "using api variables"
      (is (= 0
             (nodely/eval-node-with-values
              (nodely/cond
                (even? ?z) (+ 2 2)
                (even? ?y) (>node (nodely/cond
                                    (= 42 ?y) (- 42 ?y)
                                    (odd? ?z) :z-is-odd)))
              {:z 1, :y 42}))))
    (testing "mixing internal and api variables"
      (is (= 0
             (nodely/eval-node-with-values
              (nodely/cond
                (even? ?z) (+ 2 2)
                (even? ?y) (>node (syntax/cond
                                    (= 42 ?y) (- 42 ?y)
                                    (odd? ?z) :z-is-odd)))
              {:z 1, :y 42})))))
