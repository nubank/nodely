(ns nodely.engine.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [matcher-combinators.test :refer [match? thrown-match?]]
            [nodely.data :as data]
            [nodely.engine.core :as core]
            [nodely.fixtures :as fixtures]
            [nodely.syntax :as syntax :refer [>cond >leaf >if]]
            [schema.test]))

(use-fixtures :once schema.test/validate-schemas)

(def test-env {:a (>leaf (+ 1 2))
               :b (>leaf (* ?a 2))
               :c (>leaf (* ?a 3))
               :d (>leaf {:b ?b
                          :c ?c})})

(def test-env+cond
  (assoc test-env
    :z (>cond
        (>leaf (odd? ?c)) (>leaf [?c ?b])
        :else :nothing-else-matters)
    :target (>leaf ?z)))

(def simple-env {:x (data/value 2)
                 :y (data/leaf [:x] (fn [{:keys [x]}] (* 2 x)))
                 :z (data/leaf [:x :y] (fn [{:keys [x y]}] (+ x y)))})

(def env-with-branch {:x (data/value 2)
                      :z (data/branch (>leaf (odd? ?x))
                                      (data/value :odd)
                                      (data/value :even))})

(def branch-with-leaf {:x (data/value 2)
                       :y (data/value 3)
                       :z (data/branch (>leaf (odd? ?x))
                                       (data/value :odd)
                                       (data/leaf [:y :x]
                                                  (fn [{:keys [x y]}]
                                                    (+ x y))))})

(def branch-with-leaf-2 {:x (data/value 2)
                         :y (data/value 3)
                         :w (>leaf (+ 4 5))
                         :z (data/branch (>leaf (odd? ?x))
                                         (>leaf (inc ?w))
                                         (>leaf (+ ?w ?x ?y)))})

(def nested-branches {:x (data/value 2)
                      :y (data/value 3)
                      :z (data/branch (>leaf (odd? ?x))
                                      (data/value :x-is-odd)
                                      (data/branch (>leaf (even? ?y))
                                                   (>leaf (+ ?x ?y))
                                                   (data/value :cannot-sum-odds)))})

(def nested-branches-intermediate {:x (data/value 2)
                                   :y (data/value 3)
                                   :z (data/branch (>leaf (odd? ?x))
                                                   (>leaf [?y :x-is-odd])
                                                   (data/branch (>leaf (even? ?y))
                                                                (>leaf (+ ?x ?y))
                                                                (>leaf [?y :cannot-sum-odds])))
                                   :w (>leaf (str ?z))})

(def env-with-sequence {:x (data/value [1 2 3])
                        :y (data/sequence :x inc)})

(def branch-with-sequence {:x (data/value [1 2 3])
                           :y (data/value [4 5 6])
                           :a (data/value true)
                           :z (data/branch (>leaf ?a)
                                           (data/sequence :x inc)
                                           (data/sequence :y inc))})

(def irrelevant-expense {:x (data/value 2)
                         :y (data/value 3)
                         :z (data/branch (>leaf (odd? ?x))
                                         (data/value :odd)
                                         (data/leaf [:y :x]
                                                    (fn [{:keys [x y]}]
                                                      (+ x y))))
                         :a (data/branch (>leaf (do (Thread/sleep 60000)
                                                    true))
                                         (data/value :odd)
                                         (data/value :even))})

(def branch-depending-on-branch {:y (data/value 3)
                                 :w (>leaf (+ ?y 5))
                                 :x (data/branch
                                     (>leaf (even? ?y))
                                     (>leaf (inc ?w))
                                     (>leaf (* ?w 2)))
                                 :z (data/branch (>leaf (odd? ?x))
                                                 (>leaf (inc ?w))
                                                 (>leaf (+ ?w ?x)))})

(def branch-on-condition {:y (data/value 3)
                          :j (data/value 1)
                          :w (>leaf (+ ?y 5))
                          :x (data/branch
                              (>leaf (even? ?y))
                              (>leaf (inc ?w))
                              (>leaf (* ?w 2)))
                          :z (data/branch (data/branch (>leaf (even? ?y))
                                                       (>leaf ?j)
                                                       (>leaf (inc ?j)))
                                          (>leaf (inc ?w))
                                          (>leaf (+ ?w ?x)))})

(def env-with-cycle {:a (>leaf (inc ?b))
                     :b (>leaf (dec ?a))
                     :c (>leaf (+ ?a ?b))})

(def interesting-example {:x (data/value 22)
                          :y (data/value 13)
                          :w (data/value 1)
                          :z (data/branch (>leaf (even? ?w))
                                          (>leaf (inc ?x))
                                          (>leaf (inc ?y)))})

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

(deftest dependencies-for
  (testing "returns all transitive dependencies of a given node"
    (is (match?
         #{:a :b :c}
         (core/dependencies-for :d test-env)))))

(deftest resolve
  (testing "resolve map with two direct dependencies"
    (is (= {:x (data/value 2)
            :y (data/value 4)
            :z (data/value 6)}
           (core/resolve :z simple-env))))
  (testing "resolve map with one simple branch"
    (is (= {:x (data/value 2)
            :z (data/value :even)}
           (core/resolve :z env-with-branch))))
  (testing "resolve branch with leaf"
    (is (= {:x (data/value 2)
            :y (data/value 3)
            :z (data/value 5)}
           (core/resolve :z branch-with-leaf))))
  (testing "nested branches"
    (is (= {:x (data/value 2)
            :y (data/value 3)
            :z (data/value :cannot-sum-odds)}
           (core/resolve :z nested-branches))))
  (testing "resolve sequence"
    (is (= {:x (data/value [1 2 3])
            :y (data/value [2 3 4])}
           (core/resolve :y env-with-sequence))))
  (testing "resolve with nested branches"
    (is (= {:x (data/value 1)
            :y (data/value 2)
            :a (data/value 3)
            :b (data/value 4)
            :c (data/value 5)
            :w (data/value 4)
            :z (data/value 4)}
           (core/resolve :z tricky-example)))))

(deftest unbranch-all
  (testing "resolve only what is necessary to eliminate branches"
    (is (match? {:x (data/value 2)
                 :y (data/value 3)
                 :z  {::data/type :leaf}}
                (core/unbranch-all branch-with-leaf)))))

(deftest committed-dependencies
  (testing "Trivially returns the node dependency orders when there are no branches."
    (is (= #{:a :b :c} (core/committed-dependencies :d test-env)))
    (is (= #{:x :y} (core/committed-dependencies :z simple-env)))
    (is (= #{:x} (core/committed-dependencies :y env-with-sequence))))
  (testing "Elides irrelevant nodes"
    (is (= #{:a} (core/committed-dependencies :c test-env)))
    (is (= #{:x :z} (core/committed-dependencies :z irrelevant-expense))))
  (testing "can find common dependencies from different branch paths"
    (is (= #{:z :x :w}
           (core/committed-dependencies :z branch-with-leaf-2))))
  (testing "can find common transitive dependencies when the dependencies are also branches"
    (is (= #{:z :y :w :x}
           (core/committed-dependencies :z branch-depending-on-branch))))
  (testing "finds committed-dependencies when the branch occurs in a dependency of the target"
    (is (= #{:x :y :z} (core/committed-dependencies :w nested-branches-intermediate))))
  (testing "finds commited-dependencies when the condition of a branch is a branch"
    (is (= #{:z :j :y :w} (core/committed-dependencies :z branch-on-condition))))
  (testing "finds commited-dependencies of a branch with a sequence"
    (is (= #{:a :z} (core/committed-dependencies :z branch-with-sequence))))
  (testing "finds commited-deps of target when target is a branch"
    (is (= #{:x :z} (core/committed-dependencies :z env-with-branch)))
    (is (= #{:a :c :z} (core/committed-dependencies :target test-env+cond))))
  (testing "calculates commited deps for branch in the condition of a branch"
    (is (match? #{:x :w} (core/committed-dependencies :z tricky-example))))
  (testing "blows up with 'cant anchor graph' when env has cyclic dependency"
    (is (thrown-match? clojure.lang.ExceptionInfo
                       {}
                       (core/committed-dependencies :c env-with-cycle)))))

(defspec all-paths-for-node-doesnt-blow-up-spec
  {:num-tests 30}
  (prop/for-all [env (fixtures/env-gen {:max-branch-count 20})]
    (core/all-paths-for-node (first (keys env)) env)))

(defspec all-paths-for-node-dags-without-branches-have-only-one-path-spec
  (prop/for-all [env (fixtures/env-gen {:node-generator fixtures/scalar-gen})]
    (let [results (map #(core/all-paths-for-node % env) (keys env))]
      (every? #(= (count %) 1) results))))

(defspec commited-dependencies-doesnt-blow-up-spec
  {:num-tests 30}
  (prop/for-all [env (fixtures/env-gen {:max-branch-count 20})]
    (core/committed-dependencies (first (keys env)) env)))

(defspec commited-dependencies-of-dags-without-branches-are-the-transitive-dependencies-of-target-spec
  (prop/for-all [env (fixtures/env-gen {:node-generator fixtures/scalar-gen})]
                (every? #(= (set (core/committed-dependencies % env))
                            (core/dependencies-for % env))
                        (keys env))))

(def tricky-env-without-cycles {:d (>if (>leaf ?c) (>leaf ?e) (>leaf ?f))
                                :f (>if (>leaf ?c) :it-was-even! (>leaf ?e))
                                :e (>if (>leaf ?c) (>leaf ?f) :it-was-odd!)
                                :c (>leaf (even? (rand-int 2)))})

(deftest checked-env-throws-on-cycle-eval
  (testing "there is a cycle in the env and checked-env detects it"
    (is (thrown? clojure.lang.Compiler$CompilerException (eval `(core/checked-env env-with-cycle)))))
  (testing "there is no cycle but checked-env reports there is"
    (is (thrown? clojure.lang.Compiler$CompilerException (eval `(core/checked-env tricky-env-without-cycles))))))
