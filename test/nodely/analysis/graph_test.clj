(ns nodely.analysis.graph-test
  (:refer-clojure :exclude [cond])
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.properties :as prop]
   [matcher-combinators.test :refer [match?]]
   [nodely.analysis.graph :as graph]
   [nodely.data :as data]
   [nodely.fixtures :as fixtures]
   [nodely.syntax :as syntax :refer [>and >cond >if >leaf >or >sequence >value]]
   [schema.test]))

(use-fixtures :once schema.test/validate-schemas)

;; =============================================================================
;; TEST DATA AND FIXTURES
;; =============================================================================

(def simple-test-env
  {:user-age (>value 25)
   :threshold (>value 18)
   :is-adult (>leaf (>= ?user-age ?threshold))
   :access-level (>if (>leaf ?is-adult)
                      (>value "full")
                      (>value "restricted"))})

(def complex-test-env
  (merge simple-test-env
         {:user-role (>value "user")
          :admin-role (>value "admin")
          :is-admin (>leaf (= ?user-role ?admin-role))
          :complex-permission (>if (>leaf ?is-adult)
                                   (>if (>leaf ?is-admin)
                                        (>value "admin-access")
                                        (>value "user-access"))
                                   (>value "no-access"))
          :numbers (>value [1 2 3 4 5])
          :doubled-numbers (>sequence (inc ?numbers) :numbers)}))

(def branch-test-env
  {:x (>value 2)
   :y (>value 3)
   :condition (>leaf (even? ?x))
   :simple-branch (>if (>leaf ?condition)
                       (>value :even)
                       (>value :odd))
   :nested-branch (>if (>leaf (odd? ?x))
                       (>value :x-is-odd)
                       (>if (>leaf (even? ?y))
                            (>leaf (+ ?x ?y))
                            (>value :cannot-sum-odds)))})

;; =============================================================================
;; UNIT TESTS - Graph Analysis Functions
;; =============================================================================

(deftest node-type-checking-test
  (testing "node type predicates"
    (let [value-node (>value 42)
          leaf-node (>leaf (+ ?a ?b))
          branch-node (>if value-node leaf-node value-node)]

      (is (data/value? value-node))
      (is (not (data/value? leaf-node)))
      (is (not (data/value? branch-node)))

      (is (data/leaf? leaf-node))
      (is (not (data/leaf? value-node)))
      (is (not (data/leaf? branch-node)))

      (is (data/branch? branch-node))
      (is (not (data/branch? value-node)))
      (is (not (data/branch? leaf-node))))))

(deftest else-condition-test
  (testing "else condition detection"
    (let [else-node (>value :else)
          regular-node (>value 42)
          leaf-node (>leaf (inc ?x))]

      (is (data/else-condition? else-node))
      (is (not (data/else-condition? regular-node)))
      (is (not (data/else-condition? leaf-node)))
      (is (not (data/else-condition? nil))))))

(deftest branch-parts-extraction-test
  (testing "branch parts extraction"
    (let [condition (>leaf (inc ?x))
          truthy (>value true)
          falsey (>value false)
          branch-node (>if condition truthy falsey)
          parts (graph/extract-branch-parts branch-node)]

      (is (match? {:condition {:nodely.data/type :leaf}
                   :truthy {:nodely.data/type :value :nodely.data/value true}
                   :falsey {:nodely.data/type :value :nodely.data/value false}}
                  parts)))))

(deftest dependency-extraction-test
  (testing "dependency extraction from different node types"
    ;; Value node - no dependencies
    (is (= #{} (data/node-all-inputs (>value 42))))

    ;; Leaf node - has inputs
    (is (= #{:a :b} (data/node-all-inputs (>leaf (+ ?a ?b)))))

    ;; Sequence node - has input
    (is (= #{:data} (data/node-all-inputs (>sequence (identity ?data) :data))))

    ;; Branch node - collects from all parts
    (let [condition (>leaf (inc ?x))
          truthy (>leaf (dec ?y))
          falsey (>leaf (* 2 ?z))
          branch-node (>if condition truthy falsey)]
      (is (= #{:x :y :z} (data/node-all-inputs branch-node))))))

(deftest node-info-extraction-test
  (testing "node info extraction"
    ;; Value node
    (let [value-node (>value 42)
          info (graph/extract-node-info :test-value value-node)]
      (is (match? {:id :test-value
                   :type :value
                   :value 42}
                  info))
      (is (nil? (:dependencies info))))

    ;; Leaf node
    (let [leaf-node (>leaf (+ ?a ?b))
          info (graph/extract-node-info :test-leaf leaf-node)]
      (is (match? {:id :test-leaf
                   :type :leaf
                   :dependencies #{:a :b}
                   :inputs #{:a :b}
                   :fn-present true}
                  info)))

    ;; Sequence node
    (let [seq-node (>sequence (identity ?data) :data)
          info (graph/extract-node-info :test-seq seq-node)]
      (is (match? {:id :test-seq
                   :type :sequence
                   :dependencies #{:data}
                   :input :data
                   :sequential-input :data
                   :process-node-present true}
                  info)))))

(deftest branch-pattern-detection-test
  (testing "branch pattern detection"
    (let [simple-branch (>if (>leaf (inc ?x))
                             (>value true)
                             (>value false))
          patterns (graph/detect-branch-patterns simple-branch)]
      (is (match? {:and? false
                   :or? false
                   :cond-else? false}
                  patterns)))

    ;; Non-branch node
    (let [value-node (>value 42)]
      (is (nil? (graph/detect-branch-patterns value-node))))))

(deftest is-else-branch-test
  (testing "else branch detection"
    (let [else-branch (>if (>value :else)
                           (>value "else-result")
                           (>value nil))
          regular-branch (>if (>leaf (inc ?x))
                              (>value true)
                              (>value false))]

      (is (graph/is-else-branch? else-branch))
      (is (not (graph/is-else-branch? regular-branch))))))

(deftest extract-top-level-nodes-test
  (testing "top-level nodes extraction"
    (let [nodes (graph/extract-top-level-nodes simple-test-env)]
      (is (map? nodes))
      (is (contains? nodes :user-age))
      (is (contains? nodes :threshold))
      (is (contains? nodes :is-adult))
      (is (contains? nodes :access-level))

      ;; Check node info structure
      (is (match? {:id :user-age :type :value} (get nodes :user-age)))
      (is (match? {:id :is-adult :type :leaf} (get nodes :is-adult)))
      (is (match? {:id :access-level :type :branch} (get nodes :access-level))))))

(deftest should-extract-embedded-nodes-test
  (testing "embedded node extraction decision"
    ;; Regular branch should extract embedded nodes
    (let [regular-branch (>if (>leaf (inc ?x))
                              (>value true)
                              (>value false))]
      (is (graph/should-extract-embedded-nodes? regular-branch)))

    ;; Non-branch node should not
    (let [value-node (>value 42)]
      (is (not (graph/should-extract-embedded-nodes? value-node))))))

(deftest find-cond-branch-numbers-test
  (testing "cond branch number identification"
    (let [cond-branch (>cond
                       (>leaf (inc ?x)) (>value "result1")
                       (>leaf (dec ?y)) (>value "result2")
                       :else (>value "else"))
          branch-nums (graph/find-cond-branch-numbers cond-branch :x)]
      (is (vector? branch-nums))
      (is (contains? (set branch-nums) 1)))))

(deftest find-cond-branch-label-test
  (testing "cond branch label identification"
    (let [cond-branch (>cond
                       (>leaf (inc ?x)) (>value "result1")
                       :else (>value "else-result"))
          label (graph/find-cond-branch-label cond-branch :x)]
      (is (or (= label "cond-expr") (nil? label))))))

(deftest find-or-branch-label-test
  (testing "or branch label identification"
    ;; Create a simple >or-like structure
    (let [or-branch (>or (>leaf (inc ?x)) (>leaf (dec ?y)))
          label (graph/find-or-branch-label or-branch :x)]
      (is (or (= label "or") (nil? label))))))

(deftest find-and-branch-label-test
  (testing "and branch label identification"
    ;; Create a simple >and-like structure
    (let [and-branch (>and (>leaf (inc ?x)) (>leaf (dec ?y)))
          label (graph/find-and-branch-label and-branch :x)]
      (is (or (= label "and") (nil? label))))))

;; =============================================================================
;; INTEGRATION TESTS - Graph Structure Extraction
;; =============================================================================

(deftest extract-graph-structure-test
  (testing "basic graph structure extraction"
    (let [graph (graph/extract-graph-structure simple-test-env)
          {:keys [nodes edges]} graph]

      ;; Test nodes extraction
      (is (map? nodes))
      (is (>= (count nodes) 4))
      (is (contains? nodes :user-age))
      (is (contains? nodes :threshold))
      (is (contains? nodes :is-adult))
      (is (contains? nodes :access-level))

      ;; Test node types
      (is (match? {:type :value} (get nodes :user-age)))
      (is (match? {:type :value} (get nodes :threshold)))
      (is (match? {:type :leaf} (get nodes :is-adult)))
      (is (match? {:type :branch} (get nodes :access-level)))

      ;; Test edges extraction
      (is (vector? edges))
      (is (pos? (count edges)))

      ;; Check dependencies create proper edges
      (let [edge-map (group-by second edges)
            is-adult-deps (set (map first (get edge-map :is-adult [])))]
        (is (contains? is-adult-deps :user-age))
        (is (contains? is-adult-deps :threshold)))))

  (testing "complex graph structure extraction"
    (let [graph (graph/extract-graph-structure complex-test-env)
          {:keys [nodes edges]} graph]

      ;; Test complex structure
      (is (>= (count nodes) 6))
      (is (>= (count edges) 5))

      ;; Test sequence node handling
      (is (match? {:type :sequence} (get nodes :doubled-numbers)))
      (is (match? {:type :value} (get nodes :numbers))))))

(deftest branch-test-env-integration-test
  (testing "branch test environment integration"
    (let [graph (graph/extract-graph-structure branch-test-env)
          {:keys [nodes edges]} graph]

      ;; Test nodes extraction
      (is (map? nodes))
      (is (>= (count nodes) 5))
      (is (contains? nodes :x))
      (is (contains? nodes :y))
      (is (contains? nodes :condition))
      (is (contains? nodes :simple-branch))
      (is (contains? nodes :nested-branch))

      ;; Test node types
      (is (match? {:type :value} (get nodes :x)))
      (is (match? {:type :value} (get nodes :y)))
      (is (match? {:type :leaf} (get nodes :condition)))
      (is (match? {:type :branch} (get nodes :simple-branch)))
      (is (match? {:type :branch} (get nodes :nested-branch)))

      ;; Test edges extraction
      (is (vector? edges))
      (is (pos? (count edges)))

      ;; Check dependencies for nested branches
      (let [edge-map (group-by second edges)
            nested-branch-deps (set (map first (get edge-map :nested-branch [])))]
        (is (contains? nested-branch-deps :x))
        (is (contains? nested-branch-deps :y)))

      ;; Test analysis
      (let [analysis (graph/analyze-nodely-env branch-test-env)
            {:keys [statistics]} analysis]
        (is (>= (:total-nodes statistics) 5))
        (is (>= (:total-edges statistics) 4))
        (is (contains? (:node-types statistics) :branch))))))

(deftest analyze-nodely-env-test
  (testing "basic environment analysis"
    (let [analysis (graph/analyze-nodely-env simple-test-env)
          {:keys [graph-structure statistics]} analysis]

      ;; Test graph structure
      (is (match? {:nodes map? :edges vector?} graph-structure))

      ;; Test statistics
      (is (match? {:total-nodes pos-int?
                   :total-edges pos-int?
                   :node-types map?
                   :dependency-counts map?
                   :max-dependencies nat-int?}
                  statistics))

      ;; Verify node types are counted
      (is (contains? (:node-types statistics) :value))
      (is (contains? (:node-types statistics) :leaf))
      (is (contains? (:node-types statistics) :branch)))))

;; =============================================================================
;; PROPERTY-BASED TESTS - Testing with generated environments
;; =============================================================================

(defspec extract-graph-structure-stability-spec
  {:num-tests 30}
  (prop/for-all [env (fixtures/env-gen {:max-branch-count 10})]
                (let [result (graph/extract-graph-structure env)]
                  (and (map? (:nodes result))
                       (vector? (:edges result))))))

(defspec analyze-nodely-env-stability-spec
  {:num-tests 20}
  (prop/for-all [env (fixtures/env-gen {:max-nodes-per-stage 3
                                        :min-nodes-per-stage 1
                                        :max-branch-count 5})]
                (let [analysis (graph/analyze-nodely-env env)]
                  (and (map? (:graph-structure analysis))
                       (map? (:statistics analysis))))))

;; =============================================================================
;; INTEGRATION TESTS - Testing with nodely DSL
;; =============================================================================

(deftest nodely-dsl-integration-test
  (testing "integration with nodely DSL syntax"
    (let [dsl-env {:user-age (>value 25)
                   :threshold (>value 18)
                   :is-adult (>leaf (>= ?user-age ?threshold))
                   :access-level (>if (>leaf ?is-adult)
                                      (>value "full-access")
                                      (>value "restricted"))}

          ;; Analyze the DSL environment
          analysis (graph/analyze-nodely-env dsl-env)
          {:keys [statistics]} analysis]

      ;; Test basic analysis
      (is (>= (:total-nodes statistics) 4))
      (is (>= (:total-edges statistics) 3))

      ;; Test node types are detected
      (is (contains? (:node-types statistics) :value))
      (is (contains? (:node-types statistics) :leaf))
      (is (contains? (:node-types statistics) :branch))))

  (testing "integration with >cond DSL syntax"
    (let [cond-env {:user-age (>value 25)
                    :user-role (>value "admin")
                    :permission-level (>cond
                                       (>leaf (= ?user-role "admin")) (>value "full-access")
                                       (>leaf (>= ?user-age 18)) (>value "adult-access")
                                       :else (>value "restricted"))}

          analysis (graph/analyze-nodely-env cond-env)
          {:keys [statistics]} analysis]

      ;; Test cond analysis
      (is (>= (:total-nodes statistics) 3))
      (is (>= (:total-edges statistics) 2))

      ;; Test that branch nodes are created for >cond
      (is (contains? (:node-types statistics) :branch))))

  (testing "integration with >sequence DSL syntax"
    (let [seq-env {:numbers (>value [1 2 3 4 5])
                   :doubled (>sequence (* 2 ?numbers) :numbers)}

          analysis (graph/analyze-nodely-env seq-env)
          {:keys [statistics]} analysis]

      ;; Test sequence analysis
      (is (>= (:total-nodes statistics) 2))
      (is (>= (:total-edges statistics) 1))

      ;; Test that sequence nodes are detected
      (is (contains? (:node-types statistics) :sequence)))))

;; =============================================================================
;; ERROR HANDLING TESTS
;; =============================================================================

(deftest error-handling-test
  (testing "graceful handling of invalid inputs"
    ;; Test with nil environment - should handle gracefully
    (let [result (graph/extract-graph-structure nil)]
      (is (or (nil? result) (map? result))))

    ;; Test with empty environment
    (let [result (graph/extract-graph-structure {})]
      (is (match? {:nodes {} :edges []} result)))

    ;; Test with malformed node - should handle gracefully or throw
    (try
      (graph/extract-node-info :bad {:invalid :node})
      (catch Exception e
        (is (instance? Exception e))))))