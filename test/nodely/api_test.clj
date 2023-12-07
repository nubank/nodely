(ns nodely.api-test
  (:refer-clojure :exclude [cond])
  (:require
   [clojure.core.async :as async]
   [clojure.test :refer :all]
   [nodely.api.v0 :as api :refer [>value >leaf eval-key-channel]]))

(def env {:x (>value 2)
          :y (>value 3)
          :z (>leaf (+ ?x ?y))})

(deftest eval-key-channel-test
  (testing "returning a result to a channel with :sync.lazy"
    (is (= 5 (async/<!! (eval-key-channel env :z {::api/engine :sync.lazy})))))
  (testing "returning a result to a channel with :core-async.lazy-scheduling"
    (is (= 5 (async/<!! (eval-key-channel env :z {::api/engine :core-async.lazy-scheduling})))))
  (testing "returning a result to a channel with :applicative.core-async"
    (is (= 5 (async/<!! (eval-key-channel env :z {::api/engine :applicative.core-async}))))))

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
