(ns nodely.profile-test
  (:require
   [clojure.core.async :as core.async]
   [clojure.test :refer [deftest is testing]]
   [nodely.api.v0 :as nodely]
   [nodely.api.v0 :as api]
   [nodely.profile :as profile]))

(deftest profile-env-returns-tuple
  (testing "profile-env returns a tuple of [env atom]"
    (let [env {:a (nodely/>value 1)}
          [profiled-env profile-atom] (profile/profile-env env)]
      (is (map? profiled-env))
      (is (instance? clojure.lang.Atom profile-atom))
      (is (contains? profiled-env :a)))))

(deftest profile-leaf-records-timing
  (testing "profiled leaf nodes record execution time"
    (let [env {:a (nodely/>value 1)
               :b (nodely/>leaf (inc ?a))}
          [profiled-env profile-atom] (profile/profile-env env)
          result (nodely/eval-key profiled-env :b {::nodely/engine :sync.lazy})]
      (is (= 2 result))
      (is (contains? @profile-atom :b))
      (is (number? (get-in @profile-atom [:b :elapsed-ns])))
      (is (pos? (get-in @profile-atom [:b :elapsed-ns]))))))

(deftest profile-value-nodes-unchanged
  (testing "value nodes are not modified (no timing needed)"
    (let [env {:a (nodely/>value 42)}
          [profiled-env profile-atom] (profile/profile-env env)
          result (nodely/eval-key profiled-env :a {::nodely/engine :sync.lazy})]
      (is (= 42 result))
      ;; Value nodes don't record timing since they're immediate
      (is (not (contains? @profile-atom :a))))))

(deftest profile-branch-records-condition-and-path
  (testing "branch nodes record timing for condition and taken path"
    (let [env {:x (nodely/>value 4)
               :y (nodely/>value 100)
               :z (nodely/>if (nodely/>leaf (even? ?x))
                              (nodely/>leaf (+ ?x 1))
                              (nodely/>leaf ?y))}
          [profiled-env profile-atom] (profile/profile-env env)
          result (nodely/eval-key profiled-env :z {::nodely/engine :sync.lazy})]
      (is (= 5 result))
      ;; Should have timing for condition
      (is (contains? @profile-atom [:z :condition]))
      (is (pos? (get-in @profile-atom [[:z :condition] :elapsed-ns])))
      ;; Should have timing for truthy path (since x=4 is even)
      (is (contains? @profile-atom [:z :truthy]))
      (is (pos? (get-in @profile-atom [[:z :truthy] :elapsed-ns])))
      ;; Falsey path should NOT have timing (not evaluated)
      (is (not (contains? @profile-atom [:z :falsey]))))))

(deftest profile-branch-falsey-path
  (testing "branch nodes record timing for falsey path when condition is false"
    (let [env {:x (nodely/>value 3)
               :y (nodely/>value 100)
               :z (nodely/>if (nodely/>leaf (even? ?x))
                              (nodely/>leaf (+ ?x 1))
                              (nodely/>leaf ?y))}
          [profiled-env profile-atom] (profile/profile-env env)
          result (nodely/eval-key profiled-env :z {::nodely/engine :sync.lazy})]
      (is (= 100 result))
      ;; Should have timing for condition
      (is (contains? @profile-atom [:z :condition]))
      ;; Should have timing for falsey path (since x=3 is odd)
      (is (contains? @profile-atom [:z :falsey]))
      ;; Truthy path should NOT have timing (not evaluated)
      (is (not (contains? @profile-atom [:z :truthy]))))))

(deftest profile-sequence-with-value-process-node
  (testing "sequence nodes with value process-node (simple fn) work but don't profile the fn"
    (let [env {:items (nodely/>value [1 2 3])
               :doubled (nodely/>sequence #(* 2 %) ?items)}
          [profiled-env profile-atom] (profile/profile-env env)
          result (nodely/eval-key profiled-env :doubled {::nodely/engine :sync.lazy})]
      ;; Result should still be correct
      (is (= [2 4 6] result))
      ;; No timing recorded for value-type process nodes
      (is (not (contains? @profile-atom :doubled)))
      (is (not (contains? @profile-atom [:doubled :process]))))))

(deftest profile-nested-branches
  (testing "nested branches record timing at each level"
    (let [env {:a (nodely/>value true)
               :b (nodely/>value false)
               :x (nodely/>value 10)
               :y (nodely/>value 20)
               :z (nodely/>if (nodely/>leaf ?a)
                              (nodely/>if (nodely/>leaf ?b)
                                          (nodely/>leaf ?x)
                                          (nodely/>leaf ?y))
                              (nodely/>value 0))}
          [profiled-env profile-atom] (profile/profile-env env)
          result (nodely/eval-key profiled-env :z {::nodely/engine :sync.lazy})]
      (is (= 20 result))
      ;; Outer condition
      (is (contains? @profile-atom [:z :condition]))
      ;; Inner branch condition (truthy path of outer)
      (is (contains? @profile-atom [:z :truthy :condition]))
      ;; Inner falsey result (since b=false)
      (is (contains? @profile-atom [:z :truthy :falsey])))))

(deftest profile-multiple-leaves
  (testing "multiple leaf nodes all record timing"
    (let [env {:a (nodely/>value 1)
               :b (nodely/>leaf (+ ?a 1))
               :c (nodely/>leaf (+ ?b 1))
               :d (nodely/>leaf (+ ?c 1))}
          [profiled-env profile-atom] (profile/profile-env env)
          result (nodely/eval-key profiled-env :d {::nodely/engine :sync.lazy})]
      (is (= 4 result))
      (is (contains? @profile-atom :b))
      (is (contains? @profile-atom :c))
      (is (contains? @profile-atom :d)))))

(deftest profile-env-with-atom-uses-provided-atom
  (testing "profile-env-with-atom uses the provided atom"
    (let [my-atom (atom {:existing :data})
          env {:a (nodely/>value 1)
               :b (nodely/>leaf (inc ?a))}
          profiled-env (profile/profile-env-with-atom env my-atom)
          _ (nodely/eval-key profiled-env :b {::nodely/engine :sync.lazy})]
      ;; Should preserve existing data
      (is (= :data (:existing @my-atom)))
      ;; Should add new timing data
      (is (contains? @my-atom :b)))))

(deftest total-time-sums-all-timings
  (testing "total-time returns sum of all elapsed times"
    (let [profile-data {[:a] {:elapsed-ns 1000}
                        [:b] {:elapsed-ns 2000}
                        [:c :condition] {:elapsed-ns 500}}]
      (is (= 3500 (profile/total-time profile-data))))))

(deftest slowest-nodes-returns-sorted-results
  (testing "slowest-nodes returns nodes sorted by time descending"
    (let [profile-data {[:a] {:elapsed-ns 1000}
                        [:b] {:elapsed-ns 5000}
                        [:c] {:elapsed-ns 3000}
                        [:d] {:elapsed-ns 2000}}
          slowest (profile/slowest-nodes profile-data 3)]
      (is (= 3 (count slowest)))
      (is (= [:b] (:path (first slowest))))
      (is (= [:c] (:path (second slowest))))
      (is (= [:d] (:path (nth slowest 2)))))))

(deftest format-timing-formats-correctly
  (testing "format-timing produces human-readable output"
    (is (= "500 ns" (profile/format-timing 500)))
    (is (= "1.50 µs" (profile/format-timing 1500)))
    (is (= "2.50 ms" (profile/format-timing 2500000)))
    (is (= "1.50 s" (profile/format-timing 1500000000)))))

(deftest summarize-returns-summary-map
  (testing "summarize returns a useful summary"
    (let [profile-data {[:a] {:elapsed-ns 1000000}
                        [:b] {:elapsed-ns 2000000}
                        [:c] {:elapsed-ns 3000000}}
          summary (profile/summarize profile-data)]
      (is (= "6.00 ms" (:total-time summary)))
      (is (= 3 (:node-count summary)))
      (is (= 3 (count (:slowest summary))))
      (is (= [:c] (:path (first (:slowest summary))))))))

(deftest profile-with-slow-operations
  (testing "profiling captures meaningful timing for slow operations"
    (let [env {:a (nodely/>value 1)
               :b (nodely/>leaf (do (Thread/sleep 10) (inc ?a)))}
          [profiled-env profile-atom] (profile/profile-env env)
          _ (nodely/eval-key profiled-env :b {::nodely/engine :sync.lazy})
          elapsed-ns (get-in @profile-atom [:b :elapsed-ns])]
      ;; Should be at least 10ms (10,000,000 ns)
      (is (>= elapsed-ns 10000000)))))

(deftest profile-works-with-core-async-engine
  (testing "profiling works with core-async lazy-scheduling engine"
    (let [env {:a (nodely/>value 1)
               :b (nodely/>leaf (inc ?a))
               :c (nodely/>leaf (+ ?a ?b))}
          [profiled-env profile-atom] (profile/profile-env env)
          result (nodely/eval-key profiled-env :c {::nodely/engine :core-async.lazy-scheduling})]
      (is (= 3 result))
      (is (contains? @profile-atom :b))
      (is (contains? @profile-atom :c)))))

(def tricky-env
  {:a (nodely/>value 1)
   :b (nodely/>leaf (inc ?a))
   :d (api/>channel-leaf
       (core.async/go
         (core.async/<! (core.async/timeout 2000))
         (inc ?c)))
   :c (nodely/>leaf (+ ?a ?b))})

(deftest profile-tricky-env-with-channel-leaf
  (testing "profiling works with tricky-env containing channel-leaf nodes"
    (testing "regular leaf nodes in the dependency chain are profiled correctly"
      (let [[profiled-env profile-atom] (profile/profile-env tricky-env)
            ;; Evaluate :c which doesn't involve the channel-leaf
            result (nodely/eval-key profiled-env :d {::nodely/engine :core-async.lazy-scheduling})]
        ;; :c depends on :b and :a
        ;; Result should be (+ 1 (inc 1)) = 3
        ;; o
        ;; 
        (is (>= (get-in @profile-atom [:d :elapsed-ns]) 2000000000))
        (is (= 4 result))))))
