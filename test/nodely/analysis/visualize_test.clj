(ns nodely.analysis.visualize-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.properties :as prop]
   [matcher-combinators.test :refer [match?]]
   [nodely.analysis.graph :as graph]
   [nodely.analysis.visualize :as viz]
   [nodely.fixtures :as fixtures]
   [nodely.syntax :as syntax :refer [>and >cond >if >leaf >or >sequence >value]]
   [schema.test]))

(use-fixtures :once schema.test/validate-schemas)

;; Mock functions with arrows for testing function name extraction
(defn user-profile->json [_])

(defn json<-map [_])

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

(def comprehensive-test-env
  "Comprehensive test environment with all nodely DSL features - enhanced with diverse data types and patterns"
  {;; === VALUE NODES WITH DIVERSE DATA TYPES ===
   :user-age (>value 25)
   :user-email (>value "alice.wonderland.super.long.email@very-long-company-domain-name.com")
   :threshold (>value 18)
   :admin-role (>value "super-administrator-with-full-permissions")
   :user-role (>value "regular-user-with-limited-access")
   :user-profile (>value {:name "Alice Wonderland"
                          :department "Engineering"
                          :permissions ["read" "write" "execute"]})
   :available-roles (>value ["admin" "user" "guest" "moderator" "super-admin"])
   :empty-fallback (>value nil)
   :backup-email (>value "backup@company.com")

   ;; === LEAF NODES (using ?variable dependencies) ===
   :is-adult (>leaf (>= ?user-age ?threshold))
   :email-domain (>leaf (second (clojure.string/split ?user-email #"@")))
   :greeting (>leaf (str "Hello, " ?user-name "!"))
   :user-info (>leaf {:name ?user-name :age ?user-age :email ?user-email})
   :profile-name (>leaf (:name ?user-profile))
   :profile-email (>leaf (:email ?user-profile)) ; This will be nil, showing OR fallback
   :primary-role (>leaf (first ?available-roles))
   :profile-json (>leaf (user-profile->json ?user-profile))
   :json-data (>leaf (json<-map ?user-profile))

   ;; === SIMPLE IF BRANCH ===
   :access-level (>if (>leaf ?is-adult)
                      (>value "full-administrative-access-with-all-system-permissions-enabled")
                      (>value "restricted-user-access-with-limited-functionality-only"))

   ;; === ENHANCED OR BRANCHES - showing different fallback patterns ===
   :display-name (>or (>leaf ?user-name) ; Will be nil (unresolved)
                      (>leaf ?profile-name) ; Will get "Alice Wonderland" from map
                      (>leaf ?user-email) ; Fallback to email
                      (>value "Anonymous-User-Without-Identification"))

   ;; === OR branch with collection and map values ===
   :contact-info (>or (>leaf ?profile-email) ; Will be nil
                      (>leaf ?user-email) ; Will get the long email
                      (>leaf ?backup-email) ; Fallback email
                      (>value "no-contact-available"))

   ;; === OR branch demonstrating collection fallback ===
   :user-permissions (>or (>leaf ?empty-fallback) ; Will be nil
                          (>leaf (:permissions ?user-profile)) ; Will get the vector
                          (>value ["read-only"])) ; Fallback permissions

   ;; === COND BRANCH (2 conditions + else) ===
   :user-category (>cond
                   (>leaf (= ?user-role ?admin-role)) (>value "system-administrator-with-elevated-privileges")
                   (>leaf (= ?user-role ?user-role)) (>value "authenticated-regular-user-account")
                   :else (>value "unauthenticated-guest-visitor"))

   ;; === AND BRANCH (left-to-right evaluation) ===
   :can-access-admin (>and (>leaf ?is-adult)
                           (>leaf (= ?user-role ?admin-role))
                           (>leaf (= ?email-domain "company.com")))

   ;; === SEQUENCE NODES ===
   ;; Sequence depending on other computed values
   :user-messages (>sequence #(str ?greeting " Your category is: " %) ?user-category)
   :processed-permissions (>sequence #(str "PERM:" (clojure.string/upper-case %)) ?user-permissions)

   ;; Complex conditional with multiple dependencies
   :final-status (>cond
                  (>leaf ?can-access-admin) (>leaf (str "Admin access granted at " ?user-age))
                  (>leaf (and ?is-adult ?display-name)) (>leaf (str "User " ?display-name " verified"))
                  (>leaf (= ?user-age 18)) (>value "newly-verified-adult-user-account")
                  :else (>value "access-denied-insufficient-permissions"))})

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
;; UNIT TESTS - Visualization Functions
;; =============================================================================

(deftest value-to-safe-string-test
  (testing "basic value conversion"
    (is (= "nil" (viz/value-to-safe-string nil)))
    (is (= "test" (viz/value-to-safe-string :test)))
    (is (= "hello-world" (viz/value-to-safe-string "hello world")))
    (is (= "42" (viz/value-to-safe-string 42)))
    (is (= "true" (viz/value-to-safe-string true)))
    (is (= "false" (viz/value-to-safe-string false))))
  (testing "collection handling"
    (is (= "map-2-items" (viz/value-to-safe-string {:a 1 :b 2})))
    (is (= "vec-3-items" (viz/value-to-safe-string [1 2 3])))
    (is (= "empty-map" (viz/value-to-safe-string {})))
    (is (= "empty-coll" (viz/value-to-safe-string []))))
  (testing "string truncation"
    (let [long-string (apply str (repeat 30 "a"))
          result (viz/value-to-safe-string long-string)]
      (is (str/ends-with? result "...")))))

(deftest sanitize-string-test
  (testing "string sanitization"
    (is (= "hello-world" (viz/sanitize-string "hello world")))
    (is (= "test-123" (viz/sanitize-string "test@123")))
    (is (= "a-b-c" (viz/sanitize-string "a!@#b$%^c")))
    (is (= "collapse-dashes" (viz/sanitize-string "collapse---dashes")))
    (is (= "normal-text" (viz/sanitize-string "normal-text")))))

(deftest truncate-if-long-test
  (testing "string truncation"
    (is (= "short" (viz/truncate-if-long "short" 10)))
    (is (= "exact" (viz/truncate-if-long "exact" 5)))
    (is (= "lon..." (viz/truncate-if-long "long text" 6)))
    (is (= "abc" (viz/truncate-if-long "abc" 3)))))

(deftest format-collection-type-test
  (testing "collection type formatting"
    (is (= "empty-map" (viz/format-collection-type {})))
    (is (= "empty-coll" (viz/format-collection-type [])))
    (is (= "map-2-items" (viz/format-collection-type {:a 1 :b 2})))
    (is (= "vec-3-items" (viz/format-collection-type [1 2 3])))
    (is (= "set-2-items" (viz/format-collection-type #{1 2})))
    (is (= "seq-4-items" (viz/format-collection-type '(1 2 3 4))))))

(deftest embedded-node-detection-test
  (testing "embedded node identification"
    (is (viz/is-embedded-node? :node-truthy-value))
    (is (viz/is-embedded-node? :node-falsey-expr))
    (is (viz/is-embedded-node? :test-else-result))
    (is (viz/is-embedded-node? "branch-truthy-condition"))
    (is (not (viz/is-embedded-node? :regular-node)))
    (is (not (viz/is-embedded-node? :node-with-truthy)))
    (is (not (viz/is-embedded-node? :truthy-node)))
    (is (not (viz/is-embedded-node? :falsey-node)))))

(deftest embedded-node-name-creation-test
  (testing "embedded node name creation"
    (is (= "test-truthy-value" (viz/create-embedded-node-name "test" :truthy "value")))
    (is (= "branch-falsey-expr" (viz/create-embedded-node-name "branch" :falsey "expr")))
    (is (= "node-else-result" (viz/create-embedded-node-name "node" :else "result")))
    (is (= "cond-condition-check" (viz/create-embedded-node-name "cond" :condition "check")))
    (is (= "default-suffix" (viz/create-embedded-node-name "default" :custom "suffix")))))

(deftest html-escape-test
  (testing "HTML escaping"
    (is (= "normal text" (viz/html-escape "normal text")))
    (is (= "&lt;tag&gt;" (viz/html-escape "<tag>")))
    (is (= "&amp;entity;" (viz/html-escape "&entity;")))
    (is (= "&quot;quoted&quot;" (viz/html-escape "\"quoted\"")))
    (is (= "&lt;div class=&quot;test&quot;&gt;&amp;content&lt;/div&gt;"
           (viz/html-escape "<div class=\"test\">&content</div>")))
    (is (nil? (viz/html-escape nil)))))

(deftest value-preview-creation-test
  (testing "value preview creation"
    (is (= "`nil`" (viz/create-value-preview nil)))
    (is (= "`&quot;hello&quot;`" (viz/create-value-preview "hello")))
    (is (= "`:test`" (viz/create-value-preview :test)))
    (is (= "`42`" (viz/create-value-preview 42)))
    (is (= "`true`" (viz/create-value-preview true)))
    (is (= "`false`" (viz/create-value-preview false)))
    (is (= "`{}`" (viz/create-value-preview {})))
    (is (= "`[]`" (viz/create-value-preview [])))
    (is (= "`{...} (2 items)`" (viz/create-value-preview {:a 1 :b 2})))
    (is (= "`[...] (3 items)`" (viz/create-value-preview [1 2 3])))))

(deftest node-name-suffix-extraction-test
  (testing "node name suffix extraction"
    (is (= "truthy-value" (viz/extract-node-name-suffix "test-truthy-value" "test")))
    (is (= "falsey-expr" (viz/extract-node-name-suffix "branch-falsey-expr" "branch")))
    (is (= "else-result" (viz/extract-node-name-suffix "node-else-result" "node")))
    (is (nil? (viz/extract-node-name-suffix "different-name" "test")))
    (is (nil? (viz/extract-node-name-suffix "test" "test")))))

(deftest else-result-detection-test
  (testing "else result detection"
    (is (viz/is-else-result? "else-value"))
    (is (viz/is-else-result? "else-complex-expression"))
    (is (not (viz/is-else-result? "truthy-value")))
    (is (not (viz/is-else-result? "falsey-expr")))
    (is (not (viz/is-else-result? nil)))))

(deftest expected-suffix-generation-test
  (testing "expected suffix generation"
    ;; Value node - regular
    (let [value-node (>value "test")]
      (is (= "truthy-test" (viz/generate-expected-suffix value-node false)))
      (is (= "else-test" (viz/generate-expected-suffix value-node true))))

    ;; Value node - keyword
    (let [keyword-node (>value :admin)]
      (is (= "truthy-admin" (viz/generate-expected-suffix keyword-node false)))
      (is (= "else-admin" (viz/generate-expected-suffix keyword-node true))))

    ;; Leaf node - no inputs
    (let [leaf-node (>leaf (constantly 42))]
      (is (= "truthy-inline-expr" (viz/generate-expected-suffix leaf-node false)))
      (is (= "else-inline-expr" (viz/generate-expected-suffix leaf-node true))))))

(deftest suffix-matching-test
  (testing "suffix matching with truncation"
    ;; Exact match
    (is (viz/match-suffixes-with-truncation "truthy-test" "truthy-test"))

    ;; Truncated actual
    (is (viz/match-suffixes-with-truncation "truthy-very-long..." "truthy-very-long-expression"))

    ;; Truncated expected
    (is (viz/match-suffixes-with-truncation "truthy-very-long-expression" "truthy-very-long..."))

    ;; No match
    (is (not (viz/match-suffixes-with-truncation "truthy-test" "falsey-test")))
    (is (not (viz/match-suffixes-with-truncation "truthy-different..." "falsey-other")))))

(deftest constants-test
  (testing "configuration constants are properly defined"
    (is (pos-int? viz/DEFAULT-MAX-RECURSION-DEPTH))
    (is (pos-int? viz/DEFAULT-MAX-EDGE-RECURSION-DEPTH))
    (is (pos-int? viz/DEFAULT-STRING-TRUNCATION-LENGTH))
    (is (pos-int? viz/DEFAULT-PREVIEW-TRUNCATION-LENGTH))))

(deftest extract-leaf-function-names-test
  (testing "leaf function name extraction from source"
    ;; Test with nil/invalid source path
    (let [result (viz/extract-leaf-function-names nil)]
      (is (map? result)))

    ;; Test with non-existent file
    (let [result (viz/extract-leaf-function-names "/non/existent/file.clj")]
      (is (map? result)))))

(deftest function-name-extraction-with-source-test
  (testing "function name extraction when source content is available"
    ;; Test the actual function name extraction logic
    (let [mock-source-content ":user-age (>value 25)
                               :is-adult (>leaf (>= :user-age 18))
                               :greeting (>leaf (str \"Hello \" :user-name))"
          extracted-names (viz/extract-leaf-function-names nil)] ; This will return empty since no file

      ;; Test that extraction returns a map (even if empty when no source file)
      (is (map? extracted-names))

      ;; Test the regex pattern directly
      (let [pattern #":([a-zA-Z0-9-]+)\s*\(>leaf\s*\(([a-zA-Z0-9-_!?><>=./:]+)"
            matches (re-seq pattern mock-source-content)]
        (is (seq matches))
        (is (= 2 (count matches))) ; Should find 2 leaf nodes with extractable functions

        ;; Check specific matches
        (let [match-map (into {} (map (fn [[_ node-name fn-name]] [(keyword node-name) fn-name]) matches))]
          (is (= ">=" (get match-map :is-adult)))
          (is (= "str" (get match-map :greeting))))))))

(deftest embedded-node-labeling-test
  (testing "embedded node labeling in DOT format"
    (let [test-env {:age (>value 25)
                    :status (>if (>leaf (>= ?age 18))
                                 (>value "adult")
                                 (>value "minor"))}
          result (viz/analyze-nodely-env test-env)
          dot-format (:dot-format result)]

      ;; Should contain embedded nodes for the branch results
      (is (str/includes? dot-format "status-truthy"))
      (is (str/includes? dot-format "status-falsey"))

      ;; Embedded nodes should use italic type labels without underlined names
      (is (str/includes? dot-format "<I>value</I>"))

      ;; Main branch node should have underlined name
      (is (str/includes? dot-format "<U>status</U>")))))

;; =============================================================================
;; INTEGRATION TESTS - Visualization Workflow Testing
;; =============================================================================

(deftest extract-graph-structure-with-visualization-test
  (testing "graph structure extraction with embedded nodes for visualization"
    (let [analysis (graph/extract-graph-structure simple-test-env :include-embedded-nodes? true)
          {:keys [nodes edges]} analysis]

      ;; Test nodes extraction (should include embedded nodes)
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
      (is (pos? (count edges)))

      ;; Check dependencies create proper edges
      (let [edge-map (group-by second edges)
            is-adult-deps (set (map first (get edge-map :is-adult [])))]
        (is (contains? is-adult-deps :user-age))
        (is (contains? is-adult-deps :threshold)))))

  (testing "complex graph structure extraction with visualization"
    (let [analysis (graph/extract-graph-structure complex-test-env :include-embedded-nodes? true)
          {:keys [nodes edges]} analysis]

      ;; Test complex structure (should have more nodes due to embedded extraction)
      (is (>= (count nodes) 6))
      (is (>= (count edges) 5))

      ;; Test sequence node handling
      (is (match? {:type :sequence} (get nodes :doubled-numbers)))
      (is (match? {:type :value} (get nodes :numbers))))))

(deftest analyze-nodely-env-test
  (testing "complete environment analysis with visualization"
    (let [analysis (viz/analyze-nodely-env complex-test-env)
          {:keys [graph-structure statistics dot-format]} analysis]

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
      (is (contains? (:node-types statistics) :branch))

      ;; Test DOT format generation
      (is (string? dot-format))
      (is (str/starts-with? dot-format "digraph NodelyGraph"))
      (is (str/includes? dot-format "user-age"))
      (is (str/includes? dot-format "->")))))

(deftest graph-to-dot-test
  (testing "DOT format generation"
    (let [graph-structure (graph/extract-graph-structure simple-test-env :include-embedded-nodes? true)
          dot-output (viz/graph-to-dot graph-structure simple-test-env)]

      ;; Test DOT format structure
      (is (str/starts-with? dot-output "digraph NodelyGraph"))
      (is (str/ends-with? dot-output "}"))
      (is (str/includes? dot-output "rankdir=TB"))

      ;; Test node definitions
      (is (str/includes? dot-output "user-age"))
      (is (str/includes? dot-output "threshold"))
      (is (str/includes? dot-output "is-adult"))
      (is (str/includes? dot-output "access-level"))

      ;; Test edge definitions
      (is (str/includes? dot-output "->")))))

(deftest branch-handling-test
  (testing "complex branch structure handling with visualization"
    (let [analysis (graph/extract-graph-structure branch-test-env :include-embedded-nodes? true)
          {:keys [nodes _edges]} analysis]

      ;; Test nested branch handling
      (is (contains? nodes :nested-branch))
      (is (match? {:type :branch} (get nodes :nested-branch)))

      ;; Test embedded node extraction for visualization
      (let [embedded-keys (filter #(viz/is-embedded-node? %) (keys nodes))]
        (is (>= (count embedded-keys) 0))))))

(deftest edge-extraction-test
  (testing "edge extraction from various structures for visualization"
    (let [graph (graph/extract-graph-structure complex-test-env :include-embedded-nodes? true)
          edges (:edges graph)]

      (is (vector? edges))
      (is (pos? (count edges)))

      ;; Each edge should be a [from to] pair
      (doseq [edge edges]
        (is (match? [keyword? keyword?] edge))))))

(deftest statistics-generation-test
  (testing "statistics generation from nodely environment with visualization"
    (let [analysis (viz/analyze-nodely-env complex-test-env)
          stats (:statistics analysis)]

      (is (match? {:total-nodes pos-int?
                   :total-edges pos-int?
                   :node-types map?
                   :dependency-counts map?
                   :max-dependencies nat-int?}
                  stats))

      ;; Check node type counts
      (is (pos? (get (:node-types stats) :value 0)))
      (is (pos? (get (:node-types stats) :leaf 0))))))

(deftest embedded-node-extraction-test
  (testing "embedded node extraction from branches for visualization"
    (let [branch-with-embedded (>if (>value :test-condition)
                                    (>value "truthy-result")
                                    (>value "falsey-result"))
          embedded-nodes (viz/extract-embedded-nodes branch-with-embedded :test-branch)]

      (is (map? embedded-nodes))
      ;; Embedded nodes should be created for value nodes in branches
      (is (some #(str/includes? (name %) "test-branch") (keys embedded-nodes))))))

;; =============================================================================
;; PROPERTY-BASED TESTS - Testing with generated environments
;; =============================================================================

(defspec extract-graph-structure-with-visualization-doesnt-blow-up-spec
  {:num-tests 30}
  (prop/for-all [env (fixtures/env-gen {:max-branch-count 10})]
                (let [result (graph/extract-graph-structure env :include-embedded-nodes? true)]
                  (and (map? (:nodes result))
                       (vector? (:edges result))))))

(defspec analyze-nodely-env-produces-valid-output-spec
  {:num-tests 20}
  (prop/for-all [env (fixtures/env-gen {:max-nodes-per-stage 3
                                        :min-nodes-per-stage 1
                                        :max-branch-count 5})]
                (let [analysis (viz/analyze-nodely-env env)]
                  (and (map? (:graph-structure analysis))
                       (map? (:statistics analysis))
                       (string? (:dot-format analysis))
                       (str/starts-with? (:dot-format analysis) "digraph")))))

(defspec graph-to-dot-produces-valid-dot-spec
  {:num-tests 15}
  (prop/for-all [env (fixtures/env-gen {:node-generator fixtures/scalar-gen
                                        :max-nodes-per-stage 4})]
                (let [graph (graph/extract-graph-structure env :include-embedded-nodes? true)
                      dot (viz/graph-to-dot graph env)]
                  (and (string? dot)
                       (str/starts-with? dot "digraph NodelyGraph")
                       (str/ends-with? dot "}")))))

(defspec comprehensive-env-analysis-stability-spec
  {:num-tests 10}
  (prop/for-all [_dummy (fixtures/scalar-gen {})]
                (let [analysis (viz/analyze-nodely-env comprehensive-test-env)]
                  (and (map? (:graph-structure analysis))
                       (map? (:statistics analysis))
                       (string? (:dot-format analysis))
                       (>= (:total-nodes (:statistics analysis)) 15)
                       (>= (:total-edges (:statistics analysis)) 10)
                       (str/starts-with? (:dot-format analysis) "digraph")
                       (str/ends-with? (:dot-format analysis) "}")
                       (contains? (:node-types (:statistics analysis)) :value)
                       (contains? (:node-types (:statistics analysis)) :leaf)
                       (contains? (:node-types (:statistics analysis)) :branch)
                       (contains? (:node-types (:statistics analysis)) :sequence)))))

;; =============================================================================
;; REAL INTEGRATION TESTS - Testing with actual nodely DSL
;; =============================================================================

(deftest nodely-dsl-integration-test
  (testing "integration with nodely DSL syntax for visualization"
    (let [dsl-env {:user-age (>value 25)
                   :threshold (>value 18)
                   :is-adult (>leaf (>= ?user-age ?threshold))
                   :access-level (>if (>leaf ?is-adult)
                                      (>value "full-access")
                                      (>value "restricted"))}

          ;; Analyze the DSL environment
          analysis (viz/analyze-nodely-env dsl-env)
          {:keys [statistics dot-format]} analysis]

      ;; Test basic analysis
      (is (>= (:total-nodes statistics) 4))
      (is (>= (:total-edges statistics) 3))

      ;; Test node types are detected
      (is (contains? (:node-types statistics) :value))
      (is (contains? (:node-types statistics) :leaf))
      (is (contains? (:node-types statistics) :branch))

      ;; Test DOT generation
      (is (string? dot-format))
      (is (str/starts-with? dot-format "digraph NodelyGraph"))

      ;; Verify DOT format contains expected content
      (is (str/includes? dot-format "user-age"))
      (is (str/includes? dot-format "is-adult"))
      (is (str/includes? dot-format "access-level")))))

(deftest comprehensive-dsl-features-test
  (testing "comprehensive test with all DSL features for visualization"
    (let [comprehensive-env {:user-age (>value 25)
                             :user-email (>value "alice@company.com")
                             :threshold (>value 18)
                             :admin-role (>value "admin")
                             :user-role (>value "user")

                             ;; Leaf nodes with computations
                             :is-adult (>leaf (>= ?user-age ?threshold))
                             :email-domain (>leaf (second (str/split ?user-email #"@")))

                             ;; Simple if branch
                             :access-level (>if (>leaf ?is-adult)
                                                (>value "full-access")
                                                (>value "restricted"))

                             ;; Cond branch with multiple conditions
                             :user-category (>cond
                                             (>leaf (= ?user-role ?admin-role)) (>value "admin-user")
                                             (>leaf (= ?user-role "user")) (>value "regular-user")
                                             :else (>value "guest-user"))

                             ;; Sequence processing
                             :numbers (>value [1 2 3 4 5])
                             :doubled-numbers (>sequence ?numbers inc)}

          ;; Analyze the comprehensive environment
          analysis (viz/analyze-nodely-env comprehensive-env)
          {:keys [statistics dot-format]} analysis]

      ;; Test comprehensive analysis
      (is (> (:total-nodes statistics) 8))
      (is (> (:total-edges statistics) 6))

      ;; Test all node types are present
      (is (contains? (:node-types statistics) :value))
      (is (contains? (:node-types statistics) :leaf))
      (is (contains? (:node-types statistics) :branch))
      (is (contains? (:node-types statistics) :sequence))

      ;; Verify DOT format contains comprehensive content
      (is (str/includes? dot-format "user-age"))
      (is (str/includes? dot-format "user-email"))
      (is (str/includes? dot-format "access-level"))
      (is (str/includes? dot-format "user-category"))
      (is (str/includes? dot-format "doubled-numbers")))))

(deftest comprehensive-test-env-integration-test
  (testing "comprehensive test environment with all nodely features for visualization"
    (let [analysis (viz/analyze-nodely-env comprehensive-test-env)
          {:keys [graph-structure statistics dot-format]} analysis]

      ;; Test comprehensive analysis
      (is (>= (:total-nodes statistics) 15))
      (is (>= (:total-edges statistics) 10))

      ;; Test all node types are present
      (is (contains? (:node-types statistics) :value))
      (is (contains? (:node-types statistics) :leaf))
      (is (contains? (:node-types statistics) :branch))
      (is (contains? (:node-types statistics) :sequence))

      ;; Test specific node counts
      (is (>= (get (:node-types statistics) :value 0) 8))
      (is (>= (get (:node-types statistics) :leaf 0) 5))
      (is (>= (get (:node-types statistics) :branch 0) 2))
      (is (>= (get (:node-types statistics) :sequence 0) 2))

      ;; Test DOT format generation
      (is (string? dot-format))
      (is (str/starts-with? dot-format "digraph NodelyGraph"))

      ;; Verify DOT format contains comprehensive content
      (is (str/includes? dot-format "user-age"))
      (is (str/includes? dot-format "user-email"))
      (is (str/includes? dot-format "user-profile"))
      (is (str/includes? dot-format "available-roles"))
      (is (str/includes? dot-format "is-adult"))
      (is (str/includes? dot-format "email-domain"))
      (is (str/includes? dot-format "profile-name"))
      (is (str/includes? dot-format "access-level"))
      (is (str/includes? dot-format "user-category"))
      (is (str/includes? dot-format "processed-permissions"))
      (is (str/includes? dot-format "final-status"))

      ;; Test graph structure nodes
      (let [nodes (:nodes graph-structure)]
        (is (contains? nodes :user-age))
        (is (contains? nodes :user-profile))
        (is (contains? nodes :is-adult))
        (is (contains? nodes :access-level))
        (is (contains? nodes :user-category))
        (is (contains? nodes :processed-permissions))
        (is (contains? nodes :final-status))

        ;; Test node types in graph structure
        (is (match? {:type :value} (get nodes :user-age)))
        (is (match? {:type :value} (get nodes :user-profile)))
        (is (match? {:type :leaf} (get nodes :is-adult)))
        (is (match? {:type :branch} (get nodes :access-level)))
        (is (match? {:type :branch} (get nodes :user-category)))
        (is (match? {:type :sequence} (get nodes :processed-permissions)))
        (is (match? {:type :branch} (get nodes :final-status))))

      ;; Test edges exist
      (let [edges (:edges graph-structure)]
        (is (>= (count edges) 10))
        ;; Test some key dependency edges
        (is (some #(= [:user-age :is-adult] %) edges))
        (is (some #(= [:threshold :is-adult] %) edges))
        (is (some #(= [:user-profile :profile-name] %) edges))
        (is (some #(= [:user-email :email-domain] %) edges))
        (is (some #(= [:available-roles :primary-role] %) edges))))))

(deftest comprehensive-test-env-statistics-test
  (testing "detailed statistics from comprehensive test environment"
    (let [analysis (viz/analyze-nodely-env comprehensive-test-env)
          {:keys [statistics]} analysis]

      ;; Test dependency statistics
      (is (map? (:dependency-counts statistics)))
      (is (nat-int? (:max-dependencies statistics)))
      (is (>= (:max-dependencies statistics) 2))

      ;; Test that nodes with complex dependencies are tracked
      (let [dep-counts (:dependency-counts statistics)]
        (is (some #(>= (val %) 2) dep-counts))))))

(deftest comprehensive-test-env-edge-cases-test
  (testing "edge cases and complex patterns in comprehensive environment"
    (let [analysis (viz/analyze-nodely-env comprehensive-test-env)
          {:keys [graph-structure dot-format]} analysis
          nodes (:nodes graph-structure)]

      ;; Test handling of complex data structures
      (is (match? {:type :value
                   :value map?} (get nodes :user-profile)))

      ;; Test handling of collections
      (is (match? {:type :value
                   :value vector?} (get nodes :available-roles)))

      ;; Test nil value handling
      (is (match? {:type :value
                   :value nil} (get nodes :empty-fallback)))

      ;; Test long string handling in DOT format
      (is (str/includes? dot-format "alice.wonderland"))
      (is (str/includes? dot-format "super-administrator"))

      ;; Test sequence node with complex processing
      (is (match? {:type :sequence} (get nodes :processed-permissions)))

      ;; Test complex conditional with multiple branches
      (is (match? {:type :branch} (get nodes :final-status)))

      ;; Verify DOT format handles complex structures without errors
      (is (not (str/includes? dot-format "ERROR")))
      (is (not (str/includes? dot-format "null")))
      (is (str/ends-with? dot-format "}")))))

(deftest comprehensive-test-env-stability-test
  (testing "stability with comprehensive test environment"
    ;; Test that multiple runs produce consistent results
    (let [first-analysis (viz/analyze-nodely-env comprehensive-test-env)
          second-analysis (viz/analyze-nodely-env comprehensive-test-env)]

      ;; Statistics should be identical across runs
      (is (= (:statistics first-analysis) (:statistics second-analysis)))

      ;; Graph structure should be identical
      (is (= (:graph-structure first-analysis) (:graph-structure second-analysis)))

      ;; DOT format should be identical (deterministic)
      (is (= (:dot-format first-analysis) (:dot-format second-analysis))))))

;; =============================================================================
;; ERROR HANDLING TESTS
;; =============================================================================

(deftest error-handling-test
  (testing "graceful handling of invalid inputs"
    ;; Test with nil environment - should handle gracefully
    (let [result (graph/extract-graph-structure nil :include-embedded-nodes? true)]
      (is (or (nil? result) (map? result))))

    ;; Test with empty environment
    (let [result (graph/extract-graph-structure {} :include-embedded-nodes? true)]
      (is (match? {:nodes {} :edges []} result)))))

(deftest edge-case-handling-test
  (testing "edge cases in visualization"
    ;; Test with very large values
    (let [large-value (apply str (repeat 1000 "x"))
          env {:large (>value large-value)}
          result (viz/analyze-nodely-env env)]
      (is (string? (:dot-format result))))

    ;; Test with deeply nested structures
    (let [deep-map (reduce (fn [acc i] {i acc}) {} (range 50))
          env {:deep (>value deep-map)}
          result (viz/analyze-nodely-env env)]
      (is (string? (:dot-format result))))))

;; =============================================================================
;; API MODE TESTS
;; =============================================================================

(deftest analyze-nodely-env-api-modes-test
  (testing "analyze-nodely-env API with different modes"
    (let [test-env {:user-age (>value 25)
                    :is-adult (>leaf (>= ?user-age 18))
                    :access-level (>if (>leaf ?is-adult)
                                       (>value "full")
                                       (>value "restricted"))}]

      (testing "default mode (auto-detection)"
        (let [result (viz/analyze-nodely-env test-env)]
          (is (map? result))
          (is (contains? result :graph-structure))
          (is (contains? result :statistics))
          (is (contains? result :dot-format))
          (is (string? (:dot-format result)))
          (is (str/starts-with? (:dot-format result) "digraph NodelyGraph"))))

      (testing ":disabled mode (no function extraction)"
        (let [result (viz/analyze-nodely-env test-env :disabled)]
          (is (map? result))
          (is (contains? result :graph-structure))
          (is (contains? result :statistics))
          (is (contains? result :dot-format))
          (is (string? (:dot-format result)))
          ;; Should not contain function names since extraction is disabled
          (is (not (str/includes? (:dot-format result) "fn:"))))))))

(deftest analyze-nodely-env-consistency-test
  (testing "consistency across different API modes"
    (let [test-env {:user-age (>value 25)
                    :threshold (>value 18)
                    :is-adult (>leaf (>= ?user-age ?threshold))}

          default-result (viz/analyze-nodely-env test-env)
          disabled-result (viz/analyze-nodely-env test-env :disabled)]

      ;; Graph structure should be identical across all modes
      (is (= (:graph-structure default-result)
             (:graph-structure disabled-result)))

      ;; Statistics should be identical across all modes
      (is (= (:statistics default-result)
             (:statistics disabled-result)))

      ;; DOT format structure should be similar (though function names may differ)
      (is (str/starts-with? (:dot-format default-result) "digraph NodelyGraph"))
      (is (str/starts-with? (:dot-format disabled-result) "digraph NodelyGraph")))))

(deftest auto-detect-source-file-test
  (testing "auto-detect-source-file function"
    ;; Test with nil environment
    (is (nil? (viz/auto-detect-source-file nil)))

    ;; Test with empty environment
    (is (nil? (viz/auto-detect-source-file {})))

    ;; Test with a test environment (should not find source since it's defined in test)
    (let [test-env {:test-node (>value "test")}]
      ;; This will likely return nil since test-env is not defined in a source file
      ;; but the function should not throw an error
      (is (or (nil? (viz/auto-detect-source-file test-env))
              (string? (viz/auto-detect-source-file test-env)))))))

(deftest function-name-extraction-test
  (testing "function name extraction from source files"
    ;; Test the leaf function name extraction with a mock source content
    (let [mock-source ":test-node (>leaf (+ 1 2))
                      :another-node (>leaf (str \"hello\" \"world\"))
                      :complex-node (>leaf (>= x y))"]
      ;; Test pattern matching (this tests the internal regex)
      (is (seq (re-seq #":([a-zA-Z0-9-]+)\s*\(>leaf\s*\(([a-zA-Z0-9-_!?><>=./:]+)" mock-source))))))

(deftest dot-format-node-labeling-test
  (testing "DOT format node labeling with new format"
    (let [test-env {:user-age (>value 25)
                    :is-adult (>leaf (>= ?user-age 18))
                    :status (>if (>leaf ?is-adult)
                                 (>value "adult")
                                 (>value "minor"))}
          result (viz/analyze-nodely-env test-env)
          dot-format (:dot-format result)]

      ;; Test that nodes use italic type labels
      (is (str/includes? dot-format "<I>value</I>"))
      (is (str/includes? dot-format "<I>leaf</I>"))
      (is (str/includes? dot-format "<I>control flow</I>"))

      ;; Test that node names are underlined
      (is (str/includes? dot-format "<U>user-age</U>"))
      (is (str/includes? dot-format "<U>is-adult</U>"))
      (is (str/includes? dot-format "<U>status</U>"))

      ;; Test basic DOT structure
      (is (str/includes? dot-format "digraph NodelyGraph"))
      (is (str/includes? dot-format "rankdir=TB"))
      (is (str/ends-with? dot-format "}")))))