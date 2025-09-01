(ns nodely.analysis.graph
  (:require
   [clojure.set]
   [clojure.string :as str]
   [nodely.data :as data]))

;; Configuration constants
(def ^:const DEFAULT-MAX-RECURSION-DEPTH 100)
(def ^:const DEFAULT-MAX-EDGE-RECURSION-DEPTH 100)

;; Simple identifier utilities for embedded nodes (no visual formatting)
(defn value-to-identifier
  "Convert a value to a simple identifier string for node naming (no visual formatting)."
  [value]
  (cond
    (nil? value) "nil"
    (keyword? value) (name value)
    (string? value) value
    (symbol? value) (name value)
    (number? value) (str value)
    (boolean? value) (str value)
    (coll? value) (str "coll-" (hash value))
    :else (str value)))

(defn create-embedded-identifier
  "Create a simple identifier for embedded nodes (no visual formatting)."
  [prefix branch-type value-or-deps-str]
  (case branch-type
    :truthy (str prefix "-truthy-" value-or-deps-str)
    :falsey (str prefix "-falsey-" value-or-deps-str)
    :else (str prefix "-else-" value-or-deps-str)
    :condition (str prefix "-condition-" value-or-deps-str)
    (str prefix "-" value-or-deps-str)))

(defn is-embedded-node?
  "Check if a node name represents an embedded node (contains -truthy-, -falsey-, or -else-)."
  [node-name]
  (let [name-str (if (keyword? node-name) (name node-name) (str node-name))]
    (or (str/includes? name-str "-truthy-")
        (str/includes? name-str "-falsey-")
        (str/includes? name-str "-else-"))))

;; Node part extraction utilities

(defn extract-branch-parts
  "Extract condition, truthy, and falsey parts from a branch node."
  [node]
  {:condition (data/branch-condition node)
   :truthy (data/branch-truthy node)
   :falsey (data/branch-falsey node)})

(defn is-else-branch?
  "Check if a node represents an else branch (condition is :else)."
  [node]
  (data/else-condition? (data/branch-condition node)))

(defn find-nested-else-condition
  "Find nested else conditions up to a maximum depth."
  [node-def max-depth]
  (loop [current-node node-def depth 0]
    (when (and current-node (< depth max-depth))
      (let [falsey (data/branch-falsey current-node)
            condition (when falsey (data/branch-condition falsey))]
        (if (data/else-condition? condition)
          condition
          (recur falsey (inc depth)))))))

(defn detect-branch-patterns
  "Detect >and, >or, and >cond patterns in a branch node.
   Returns a map with :and?, :or?, :cond-else? keys."
  [node-def]
  (when (data/branch? node-def)
    (let [{:keys [condition truthy falsey]} (extract-branch-parts node-def)

          ;; Check for deeply nested else conditions to detect >cond
          is-cond-else? (boolean (find-nested-else-condition node-def 3))

          ;; Detect >and pattern: condition and falsey are identical, truthy contains next condition
          condition-inputs (when condition (data/node-inputs condition))
          falsey-inputs (when falsey (data/node-inputs falsey))
          is-and-pattern? (and condition truthy falsey
                               (= condition-inputs falsey-inputs)
                               (data/branch? truthy)
                               (not is-cond-else?))

          ;; Detect >or pattern: condition and truthy are identical, falsey contains next option
          truthy-inputs (when truthy (data/node-inputs truthy))
          is-or-pattern? (and condition truthy falsey
                              (= condition-inputs truthy-inputs)
                              (data/branch? falsey)
                              (not is-cond-else?))]

      {:and? is-and-pattern?
       :or? is-or-pattern?
       :cond-else? is-cond-else?})))

;; Graph extraction functions

(defn extract-node-info
  "Extract information about a single node including its type and dependencies."
  [node-key node-def]
  (let [node-type (data/node-type node-def)
        base-info {:id node-key
                   :type node-type
                   :dependencies (data/node-all-inputs node-def)}]
    (case node-type
      :value (-> base-info
                 (dissoc :dependencies)
                 (assoc :value (data/node-value node-def)))
      :leaf (assoc base-info
                   :fn-present (boolean (::data/fn node-def))
                   :inputs (data/node-inputs node-def))
      :branch (let [condition (data/branch-condition node-def)
                    truthy (data/branch-truthy node-def)
                    falsey (data/branch-falsey node-def)]
                (assoc base-info
                       :condition-deps (if condition (data/node-inputs condition) #{})
                       :truthy-deps (if truthy (data/node-inputs truthy) #{})
                       :falsey-deps (if falsey (data/node-inputs falsey) #{})))
      :sequence (assoc base-info
                       :process-node-present (boolean (::data/process-node node-def))
                       :input (data/sequence-input node-def)
                       :sequential-input (data/sequence-input node-def)) ; sequence nodes have one sequential input
      :unresolved (assoc base-info :value nil)
      base-info)))

(defn extract-top-level-nodes
  "Extract top-level nodes from environment, converting to node-info structures."
  [env]
  (into {} (map (fn [[k v]] [k (extract-node-info k v)]) env)))

(defn should-extract-embedded-nodes?
  "Check if we should extract embedded nodes for a given branch node."
  [node-def]
  (when (data/branch? node-def)
    (let [{:keys [and? or?]} (detect-branch-patterns node-def)]
      ;; Only extract embedded nodes for IF and COND branches, not AND/OR
      (not (or and? or?)))))

(defn collect-all-dependencies
  "Collect all dependencies from top-level and embedded nodes."
  [top-level-nodes embedded-nodes]
  (apply clojure.set/union
         (for [[_ node-info] (merge top-level-nodes embedded-nodes)]
           (:dependencies node-info))))

(defn find-unresolved-dependencies
  "Find dependencies that don't exist as nodes."
  [all-dependencies top-level-nodes embedded-nodes]
  (let [all-node-ids (set (keys (merge top-level-nodes embedded-nodes)))]
    (clojure.set/difference all-dependencies all-node-ids)))

(defn create-unresolved-node-entries
  "Create node entries for unresolved dependencies."
  [unresolved-deps]
  (into {} (for [dep unresolved-deps]
             [dep {:id dep
                   :type :unresolved
                   :dependencies #{}}])))

(defn find-cond-branch-numbers
  "Find which branch number(s) a dependency belongs to in a >cond structure.
   Returns a vector of branch numbers (starting from 1)."
  [node from]
  (letfn [(find-nums [node from branch-num acc]
            (let [current-condition (data/branch-condition node)
                  condition-inputs (if-let [condition (data/branch-condition node)] (data/node-inputs condition) #{})
                  truthy-inputs (if-let [truthy (data/branch-truthy node)] (data/node-inputs truthy) #{})
                  falsey-node (data/branch-falsey node)
                  falsey-inputs (if-let [falsey (data/branch-falsey node)] (data/node-inputs falsey) #{})
                  is-else-branch? (data/else-condition? current-condition)

                  ;; Check if dependency is in current branch
                  in-condition? (contains? condition-inputs from)
                  in-truthy? (contains? truthy-inputs from)
                  in-falsey? (contains? falsey-inputs from)

                  ;; Collect branch numbers for current level
                  current-nums (cond
                                 ;; If it's an else branch, don't assign a number (it's the final else)
                                 is-else-branch? (if (or in-condition? in-truthy?) ["else"] [])
                                 ;; If dependency is in condition or truthy, it belongs to current branch
                                 (or in-condition? in-truthy?) [branch-num]
                                 ;; If in falsey (and not else), it's also current branch for some cases
                                 in-falsey? [branch-num]
                                 :else [])

                  updated-acc (concat acc current-nums)]

              ;; Recursively check nested branches (tail call optimized)
              (if (and falsey-node (data/branch? falsey-node))
                (find-nums falsey-node from (if is-else-branch? branch-num (inc branch-num)) updated-acc)
                updated-acc)))]
    (vec (find-nums node from 1 []))))

(defn find-cond-branch-label
  "Find the appropriate label for a dependency in a >cond branch structure.
   >cond creates nested >if structures with :else markers."
  [node from]
  (letfn [(find-label [node from branch-num]
            (let [current-condition (data/branch-condition node)
                  condition-inputs (if-let [condition (data/branch-condition node)] (data/node-inputs condition) #{})
                  truthy-inputs (if-let [truthy (data/branch-truthy node)] (data/node-inputs truthy) #{})
                  falsey-node (data/branch-falsey node)
                  falsey-inputs (if-let [falsey (data/branch-falsey node)] (data/node-inputs falsey) #{})
                  is-else-branch? (data/else-condition? current-condition)]
              (cond
                (contains? condition-inputs from)
                (if is-else-branch? "else" "cond-expr")

                (contains? truthy-inputs from)
                (if is-else-branch? "else" "cond-expr")

                (and falsey-node (data/branch? falsey-node))
                (find-label falsey-node from (inc branch-num))

                (contains? falsey-inputs from)
                "else"

                :else nil)))]
    (find-label node from 1)))

(defn find-or-branch-label
  "Find the appropriate label for a dependency in an >or branch structure.
   >or evaluates left-to-right and returns the first truthy value.
   Structure: condition=truthy (first option), falsey contains next option."
  [node from]
  (letfn [(find-label [node from branch-num]
            (let [condition-inputs (if-let [condition (data/branch-condition node)] (data/node-inputs condition) #{})
                  truthy-inputs (if-let [truthy (data/branch-truthy node)] (data/node-inputs truthy) #{})
                  falsey-node (data/branch-falsey node)
                  falsey-inputs (if-let [falsey (data/branch-falsey node)] (data/node-inputs falsey) #{})]
              (cond
                ;; Check if dependency is in the current option (condition/truthy should be same)
                (contains? condition-inputs from)
                "or"

                (contains? truthy-inputs from)
                "or"

                ;; If falsey is a branch, recurse to next option
                (and falsey-node (data/branch? falsey-node))
                (find-label falsey-node from (inc branch-num))

                ;; If falsey is a value (final fallback), check if it matches
                (contains? falsey-inputs from)
                "or"

                :else nil)))]
    (find-label node from 1)))

(defn find-and-branch-label
  "Find the appropriate label for a dependency in an >and branch structure.
   >and evaluates left-to-right and returns the first falsy value, or the last value if all are truthy.
   Structure: condition=falsey (current check), truthy contains next condition."
  [node from]
  (letfn [(find-label [node from branch-num]
            (let [condition-inputs (if-let [condition (data/branch-condition node)] (data/node-inputs condition) #{})
                  truthy-node (data/branch-truthy node)
                  truthy-inputs (if-let [truthy (data/branch-truthy node)] (data/node-inputs truthy) #{})
                  falsey-inputs (if-let [falsey (data/branch-falsey node)] (data/node-inputs falsey) #{})

                  ;; Also check nested dependencies in truthy branch
                  all-truthy-deps (if truthy-node (data/node-all-inputs truthy-node) #{})]
              (cond
                ;; Check if dependency is in the current condition
                (contains? condition-inputs from)
                "and"

                ;; Check if dependency is in the falsey (same as condition for >and)
                (contains? falsey-inputs from)
                "and"

                ;; Check if dependency is anywhere in the truthy branch (including nested)
                (contains? all-truthy-deps from)
                (if (and truthy-node (data/branch? truthy-node))
                  ;; If truthy is a branch, recurse to get the proper nested label
                  (let [recursive-result (find-label truthy-node from (inc branch-num))]
                    (or recursive-result "and"))
                  ;; If truthy is a leaf, it's the next condition
                  "and")

                ;; If truthy is a leaf (final result), check if it matches directly
                (contains? truthy-inputs from)
                "and"

                :else nil)))]
    (find-label node from 1)))

;; Embedded node extraction functions

(defn create-embedded-node-entry
  "Create embedded node entry with clean name generation."
  [node prefix branch-type identifier-fn]
  (let [identifier (identifier-fn node)
        clean-name (create-embedded-identifier prefix branch-type identifier)]
    {(keyword clean-name) node}))

(defn create-embedded-value-node
  "Create embedded node entry for a value node."
  [node prefix branch-type]
  (create-embedded-node-entry node prefix branch-type
                              (fn [node] (value-to-identifier (data/node-value node)))))

(defn create-embedded-leaf-node
  "Create embedded node entry for a leaf node."
  [node prefix branch-type]
  (create-embedded-node-entry node prefix branch-type
                              (fn [node]
                                (let [deps (data/node-inputs node)]
                                  (if (seq deps)
                                    (str/join "-" (map name deps))
                                    "inline-expr")))))

(defn should-extract-condition-node?
  "Check if a condition node should be extracted as embedded."
  [condition-node is-else-branch?]
  (and condition-node
       (data/leaf? condition-node)
       (not is-else-branch?)
       (let [deps (data/node-inputs condition-node)
             has-complex-logic? (> (count deps) 2)]
         has-complex-logic?)))

(defn extract-condition-embedded
  "Extract embedded nodes from condition part of branch."
  [condition-node prefix depth is-else-branch? extract-values-fn]
  (when (should-extract-condition-node? condition-node is-else-branch?)
    (extract-values-fn condition-node prefix :condition (inc depth))))

(defn should-extract-truthy-directly?
  "Check if truthy node should be extracted directly."
  [truthy-node]
  (or (data/value? truthy-node)
      (data/leaf? truthy-node)))

(defn extract-nested-truthy-result
  "Extract result from nested truthy branch."
  [truthy-node prefix depth extract-values-fn]
  (let [nested-truthy (data/branch-truthy truthy-node)]
    (when (and nested-truthy
               (or (= nested-truthy :value)
                   (= nested-truthy :leaf)))
      (extract-values-fn nested-truthy prefix :truthy (inc depth)))))

(defn extract-truthy-embedded
  "Extract embedded nodes from truthy part of branch."
  [truthy-node prefix depth is-else-branch? extract-values-fn]
  (when truthy-node
    (cond
      (should-extract-truthy-directly? truthy-node)
      (extract-values-fn truthy-node prefix
                         (if is-else-branch? :else :truthy)
                         (inc depth))

      (data/branch? truthy-node)
      (extract-nested-truthy-result truthy-node prefix depth extract-values-fn)

      :else nil)))

(defn should-skip-nil-falsey?
  "Check if falsey node should be skipped (nil placeholder)."
  [falsey-node]
  (and (= (data/node-type falsey-node) :value)
       (nil? (data/node-value falsey-node))))

(defn should-extract-falsey-directly?
  "Check if falsey node should be extracted directly."
  [falsey-node]
  (or (= (data/node-type falsey-node) :value)
      (= (data/node-type falsey-node) :leaf)))

(defn should-recurse-falsey?
  "Check if we should recurse into falsey branch."
  [depth]
  ;; Defensive depth limit to prevent infinite recursion
  ;; TODO: Cycle detection would be even better than this depth limit
  (< depth DEFAULT-MAX-RECURSION-DEPTH))

(defn extract-falsey-embedded
  "Extract embedded nodes from falsey part of branch."
  [falsey-node prefix depth extract-values-fn]
  (when falsey-node
    (if (should-extract-falsey-directly? falsey-node)
      (when (not (should-skip-nil-falsey? falsey-node))
        (extract-values-fn falsey-node prefix :falsey (inc depth)))
      (when (should-recurse-falsey? depth)
        (extract-values-fn falsey-node prefix :falsey (inc depth))))))

(defn extract-embedded-nodes
  "Recursively extract embedded nodes from branch structures.
   Returns a map of {node-id -> node-def} for all embedded nodes."
  [node-def node-id-prefix]
  (let [extract-values
        (fn extract-values [node prefix branch-type depth]
          (case (data/node-type node)
            :value (create-embedded-value-node node prefix branch-type)
            :leaf (create-embedded-leaf-node node prefix branch-type)
            :branch
            (let [condition-node (data/branch-condition node)
                  truthy-node (data/branch-truthy node)
                  falsey-node (data/branch-falsey node)
                  is-else-branch? (is-else-branch? node)

                  condition-embedded (extract-condition-embedded condition-node prefix depth is-else-branch? extract-values)
                  truthy-embedded (extract-truthy-embedded truthy-node prefix depth is-else-branch? extract-values)
                  falsey-embedded (extract-falsey-embedded falsey-node prefix depth extract-values)]

              (merge condition-embedded truthy-embedded falsey-embedded))
            {}))

        prefix (name node-id-prefix)]

    (extract-values node-def prefix :root 0)))

(defn extract-filtered-embedded-nodes
  "Extract embedded nodes from branch structures, filtering by branch patterns."
  [env]
  (let [embedded-nodes-raw (apply merge
                                  (for [[node-id node-def] env
                                        :when (should-extract-embedded-nodes? node-def)]
                                    (extract-embedded-nodes node-def node-id)))]
    (into {} (map (fn [[k v]] [k (extract-node-info k v)]) embedded-nodes-raw))))

(defn should-include-dependency-edge?
  "Check if a dependency edge should be included, considering embedded node handling."
  [node-id node-info dep embedded-nodes]
  (let [is-branch? (= (:type node-info) :branch)
        ;; For non-branch nodes, check if embedded nodes handle this dependency
        embedded-handles-dep? (and (not is-branch?)
                                   (let [node-name (name node-id)]
                                     (some (fn [[embedded-id embedded-info]]
                                             (let [embedded-name (name embedded-id)]
                                               (and (str/starts-with? embedded-name (str node-name "-"))
                                                    (contains? (:dependencies embedded-info) dep))))
                                           embedded-nodes)))]
    ;; Include edge unless it's handled by embedded nodes for non-branch nodes
    (not embedded-handles-dep?)))

(defn extract-dependency-edges-with-filtering
  "Extract regular dependency edges with smart routing for embedded nodes."
  [all-nodes embedded-nodes]
  (for [[node-id node-info] all-nodes
        dep (:dependencies node-info)
        :when (should-include-dependency-edge? node-id node-info dep embedded-nodes)]
    [dep node-id]))

;; Branch result edge extraction functions

(defn should-create-condition-edge?
  "Check if we should create an edge from condition node."
  [condition-node is-else-branch?]
  (and condition-node
       (= (data/node-type condition-node) :leaf)
       (not is-else-branch?)
       (let [deps (data/node-inputs condition-node)
             has-complex-logic? (> (count deps) 2)]
         has-complex-logic?)))

(defn create-condition-edge
  "Create edge from condition node to branch."
  [condition-node prefix node-id embedded-nodes]
  (let [deps (data/node-inputs condition-node)
        deps-str (if (seq deps)
                   (str/join "-" (map name deps))
                   "inline-expr")
        condition-key (keyword (str prefix "-condition-" deps-str))]
    (when (contains? embedded-nodes condition-key)
      [[condition-key node-id]])))

(defn create-edge-key
  "Create a keyword for edge identification based on prefix, branch type, and identifier."
  [prefix branch-type identifier]
  (keyword (str prefix "-" (name branch-type) "-" identifier)))

(defn extract-condition-edges
  "Extract edges from embedded condition nodes to branch."
  [condition-node prefix node-id embedded-nodes is-else-branch?]
  (when (should-create-condition-edge? condition-node is-else-branch?)
    (create-condition-edge condition-node prefix node-id embedded-nodes)))

(defn create-value-node-key
  "Create key for value node edge."
  [value prefix is-else-branch?]
  (let [value-str (value-to-identifier value)
        branch-type (if is-else-branch? :else :truthy)]
    (create-edge-key prefix branch-type value-str)))

(defn create-leaf-node-key
  "Create key for leaf node edge."
  [deps prefix is-else-branch?]
  (let [deps-str (if (seq deps)
                   (str/join "-" (map name deps))
                   "inline-expr")
        branch-type (if is-else-branch? :else :truthy)]
    (create-edge-key prefix branch-type deps-str)))

(defn create-truthy-edge-direct
  "Create edge from truthy node (direct value/leaf) to branch."
  [truthy-node prefix node-id embedded-nodes is-else-branch?]
  (let [truthy-key (if (= (data/node-type truthy-node) :value)
                     (create-value-node-key (data/node-value truthy-node) prefix is-else-branch?)
                     (create-leaf-node-key (data/node-inputs truthy-node) prefix is-else-branch?))]
    (when (contains? embedded-nodes truthy-key)
      [[truthy-key node-id]])))

(defn create-nested-truthy-key
  "Create key for nested truthy node."
  [nested-truthy prefix]
  (let [identifier (if (= nested-truthy :value)
                     (value-to-identifier (data/node-value nested-truthy))
                     (let [deps (data/node-inputs nested-truthy)]
                       (if (seq deps)
                         (str/join "-" (map name deps))
                         "inline-expr")))]
    (create-edge-key prefix :truthy identifier)))

(defn create-truthy-edge-nested
  "Create edge from nested truthy branch result to branch."
  [truthy-node prefix node-id embedded-nodes]
  (let [nested-truthy (data/branch-truthy truthy-node)]
    (when (and nested-truthy
               (or (= nested-truthy :value)
                   (= nested-truthy :leaf)))
      (let [nested-key (create-nested-truthy-key nested-truthy prefix)]
        (when (contains? embedded-nodes nested-key)
          [[nested-key node-id]])))))

(defn should-create-truthy-edge-direct?
  "Check if truthy node should create direct edge."
  [truthy-node]
  (or (= (data/node-type truthy-node) :value)
      (= (data/node-type truthy-node) :leaf)))

(defn extract-truthy-edges
  "Extract edges from truthy nodes to branch."
  [truthy-node prefix node-id embedded-nodes is-else-branch?]
  (when truthy-node
    (cond
      (should-create-truthy-edge-direct? truthy-node)
      (create-truthy-edge-direct truthy-node prefix node-id embedded-nodes is-else-branch?)

      (data/branch? truthy-node)
      (create-truthy-edge-nested truthy-node prefix node-id embedded-nodes)

      :else nil)))

(defn should-skip-falsey-edge?
  "Check if falsey edge should be skipped (nil placeholder)."
  [falsey-node]
  (and (= (data/node-type falsey-node) :value)
       (nil? (data/node-value falsey-node))))

(defn create-falsey-edge-key
  "Create key for falsey node edge."
  [falsey-node prefix]
  (let [identifier (if (= (data/node-type falsey-node) :value)
                     (value-to-identifier (data/node-value falsey-node))
                     (let [deps (data/node-inputs falsey-node)]
                       (if (seq deps)
                         (str/join "-" (map name deps))
                         "inline-expr")))]
    (create-edge-key prefix :falsey identifier)))

(defn extract-falsey-edges
  "Extract edges from falsey nodes to branch."
  [falsey-node prefix node-id embedded-nodes]
  (when (and falsey-node
             (or (= (data/node-type falsey-node) :value)
                 (= (data/node-type falsey-node) :leaf))
             (not (should-skip-falsey-edge? falsey-node)))
    (let [falsey-key (create-falsey-edge-key falsey-node prefix)]
      (when (contains? embedded-nodes falsey-key)
        [[falsey-key node-id]]))))

(defn should-recurse-nested-edges?
  "Check if we should recurse into nested branch edges."
  [falsey-node depth]
  (and falsey-node
       (data/branch? falsey-node)
       (< depth DEFAULT-MAX-EDGE-RECURSION-DEPTH)))

(defn extract-nested-edges
  "Extract edges from nested branches recursively."
  [falsey-node node-id prefix depth extract-edges-fn]
  (when (should-recurse-nested-edges? falsey-node depth)
    (extract-edges-fn node-id falsey-node prefix (inc depth))))

(defn extract-branch-result-edges
  "Extract edges from embedded result nodes to their parent branch nodes.
   Returns a vector of [from to] edges where embedded nodes flow INTO branches."
  [env embedded-nodes]
  (let [extract-edges
        (fn extract-edges [node-id node-def prefix depth]
          (case (data/node-type node-def)
            :branch
            (let [condition-node (data/branch-condition node-def)
                  truthy-node (data/branch-truthy node-def)
                  falsey-node (data/branch-falsey node-def)
                  is-else-branch? (is-else-branch? node-def)

                  condition-edges (extract-condition-edges condition-node prefix node-id embedded-nodes is-else-branch?)
                  truthy-edges (extract-truthy-edges truthy-node prefix node-id embedded-nodes is-else-branch?)
                  falsey-edges (extract-falsey-edges falsey-node prefix node-id embedded-nodes)
                  nested-edges (extract-nested-edges falsey-node node-id prefix depth extract-edges)]

              (concat condition-edges truthy-edges falsey-edges nested-edges))
            []))]

    (vec (apply concat
                (for [[node-id node-def] env
                      :when (= (data/node-type node-def) :branch)]
                  (extract-edges node-id node-def (name node-id) 0))))))

(defn should-include-result-edge?
  "Check if a result edge should be included based on branch patterns."
  [to env]
  (let [to-node-def (get env to)]
    ;; Check if this is an AND or OR branch
    (if (and to-node-def (= (data/node-type to-node-def) :branch))
      (let [{:keys [and? or?]} (detect-branch-patterns to-node-def)]
        ;; For AND and OR branches, don't include embedded result edges
        (not (or and? or?)))
      ;; For non-branch targets, always include
      true)))

(defn extract-filtered-result-edges
  "Extract branch result edges filtered by branch patterns."
  [env embedded-nodes]
  (let [all-result-edges (extract-branch-result-edges env embedded-nodes)]
    (filter (fn [[_from to]]
              (should-include-result-edge? to env))
            all-result-edges)))

(defn extract-graph-structure
  "Extract the complete graph structure from a nodely environment.
   Returns {:nodes {node-id -> node-info}, :edges [[from to] ...]}

   Options:
   - :include-embedded-nodes? - Whether to extract embedded nodes from branch structures (default: false)"
  [env & {:keys [include-embedded-nodes?] :or {include-embedded-nodes? false}}]
  (let [;; Extract top-level nodes
        top-level-nodes (extract-top-level-nodes env)

        ;; Extract embedded nodes from branch structures if requested
        embedded-nodes (if include-embedded-nodes?
                         (extract-filtered-embedded-nodes env)
                         {})

        ;; Find all dependencies and unresolved ones
        all-dependencies (collect-all-dependencies top-level-nodes embedded-nodes)
        unresolved-deps (find-unresolved-dependencies all-dependencies top-level-nodes embedded-nodes)
        unresolved-nodes (create-unresolved-node-entries unresolved-deps)

        ;; Combine all nodes for edge extraction
        all-nodes (merge top-level-nodes embedded-nodes unresolved-nodes)

        ;; Extract edges with filtering if embedded nodes are included
        dependency-edges (if include-embedded-nodes?
                           (extract-dependency-edges-with-filtering all-nodes embedded-nodes)
                           (for [[node-id node-info] all-nodes
                                 dep (:dependencies node-info)]
                             [dep node-id]))

        ;; Extract branch result edges if embedded nodes are included
        result-edges (if include-embedded-nodes?
                       (extract-filtered-result-edges env embedded-nodes)
                       [])]

    {:nodes all-nodes
     :edges (vec (concat dependency-edges result-edges))}))

(defn analyze-nodely-env
  "Comprehensive analysis of a nodely environment."
  [env]
  (let [graph-structure (extract-graph-structure env)
        nodes (:nodes graph-structure)
        edges (:edges graph-structure)
        node-types (frequencies (map #(:type (second %)) nodes))
        dependency-counts (frequencies (map first edges))
        statistics {:total-nodes (count nodes)
                    :total-edges (count edges)
                    :node-types node-types
                    :dependency-counts dependency-counts
                    :max-dependencies (if (seq dependency-counts)
                                        (apply max (vals dependency-counts))
                                        0)}]
    {:graph-structure graph-structure
     :statistics statistics}))