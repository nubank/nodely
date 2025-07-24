(ns nodely.analysis.visualize
  (:require
   [clojure.set]
   [clojure.string :as str]
   [nodely.analysis.graph :as graph]
   [nodely.data :as data]))

;; Configuration constants
(def ^:const DEFAULT-MAX-RECURSION-DEPTH 100)
(def ^:const DEFAULT-MAX-EDGE-RECURSION-DEPTH 100)
(def ^:const DEFAULT-STRING-TRUNCATION-LENGTH 20)
(def ^:const DEFAULT-PREVIEW-TRUNCATION-LENGTH 25)

;; String formatting utilities for visualization
(defn sanitize-string
  "Replace non-alphanumeric characters with dashes and collapse multiple dashes."
  [s]
  (-> s
      (str/replace #"[^a-zA-Z0-9-_]" "-")
      (str/replace #"-+" "-")))

(defn truncate-if-long
  "Truncate string if longer than max-length, adding ellipsis."
  [s max-length]
  (if (> (count s) max-length)
    (str (subs s 0 (- max-length 3)) "...")
    s))

(defn format-collection-type
  "Format collection types with item counts."
  [coll]
  (cond
    (empty? coll) (if (map? coll) "empty-map" "empty-coll")
    (map? coll) (str "map-" (count coll) "-items")
    (vector? coll) (str "vec-" (count coll) "-items")
    (set? coll) (str "set-" (count coll) "-items")
    (seq? coll) (str "seq-" (count coll) "-items")
    :else (str "coll-" (count coll) "-items")))

(defn value-to-safe-string
  "Convert a value to a safe string representation for node naming."
  [value]
  (cond
    (nil? value) "nil"
    (keyword? value) (name value)
    (string? value) (-> value
                        sanitize-string
                        (truncate-if-long DEFAULT-STRING-TRUNCATION-LENGTH))
    (symbol? value) (name value)
    (number? value) (str value)
    (boolean? value) (str value)
    (coll? value) (format-collection-type value)
    :else (sanitize-string (str value))))

(defn create-embedded-node-name
  "Create a clean node name based on branch type and value/dependencies."
  [prefix branch-type value-or-deps-str]
  (case branch-type
    :truthy (str prefix "-truthy-" value-or-deps-str)
    :falsey (str prefix "-falsey-" value-or-deps-str)
    :else (str prefix "-else-" value-or-deps-str)
    :condition (str prefix "-condition-" value-or-deps-str)
    (str prefix "-" value-or-deps-str)))

;; Graph extraction functions for visualization

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
               (or (data/value? nested-truthy)
                   (data/leaf? nested-truthy)))
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
  (and (data/value? falsey-node)
       (nil? (data/node-value falsey-node))))

(defn should-extract-falsey-directly?
  "Check if falsey node should be extracted directly."
  [falsey-node]
  (or (data/value? falsey-node)
      (data/leaf? falsey-node)))

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
            :value (graph/create-embedded-value-node node prefix branch-type)
            :leaf (graph/create-embedded-leaf-node node prefix branch-type)
            :branch
            (let [condition-node (data/branch-condition node)
                  truthy-node (data/branch-truthy node)
                  falsey-node (data/branch-falsey node)
                  is-else-branch? (graph/is-else-branch? node)

                  condition-embedded (extract-condition-embedded condition-node prefix depth is-else-branch? extract-values)
                  truthy-embedded (extract-truthy-embedded truthy-node prefix depth is-else-branch? extract-values)
                  falsey-embedded (extract-falsey-embedded falsey-node prefix depth extract-values)]

              (merge condition-embedded truthy-embedded falsey-embedded))
            {}))

        prefix (name node-id-prefix)]

    (extract-values node-def prefix :root 0)))

(defn extract-node-name-suffix
  "Extract the suffix after the branch name from an embedded node name.
   e.g., 'final-status-truthy-user-age' -> 'truthy-user-age'"
  [to-name from-name]
  (when (str/starts-with? to-name (str from-name "-"))
    (subs to-name (+ (count from-name) 1))))

(defn is-else-result?
  "Check if a suffix indicates an else result."
  [suffix]
  (and suffix (str/starts-with? suffix "else-")))

(defn generate-expected-suffix
  "Generate expected suffix based on the truthy node type and content."
  [truthy-node is-else-branch?]
  (if (= (data/node-type truthy-node) :value)
    ;; For value nodes, create safe string representation
    (let [value (data/node-value truthy-node)
          value-str (value-to-safe-string value)]
      (if is-else-branch?
        (str "else-" value-str)
        (str "truthy-" value-str)))
    ;; For leaf nodes, use dependencies
    (let [deps (data/node-inputs truthy-node)
          deps-str (if (seq deps)
                     (str/join "-" (map name deps))
                     "inline-expr")]
      (if is-else-branch?
        (str "else-" deps-str)
        (str "truthy-" deps-str)))))

(defn match-suffixes-with-truncation
  "Compare actual suffix with expected suffix, handling truncation cases."
  [suffix expected-suffix]
  (or (= suffix expected-suffix)
      (and (str/ends-with? suffix "...")
           (str/starts-with? expected-suffix (str/replace suffix "..." "")))
      (and (str/ends-with? expected-suffix "...")
           (str/starts-with? suffix (str/replace expected-suffix "..." "")))))

(defn check-truthy-match
  "Check if current branch's truthy result matches the target suffix."
  [truthy-node suffix is-else-branch?]
  (and truthy-node
       (or (= (data/node-type truthy-node) :value)
           (= (data/node-type truthy-node) :leaf))
       (let [expected-suffix (generate-expected-suffix truthy-node is-else-branch?)]
         (match-suffixes-with-truncation suffix expected-suffix))))

(defn determine-nested-branch-number
  "Determine the branch number for nested branches in cond structures."
  [nested-condition branch-num]
  (let [nested-is-else? (and nested-condition
                             (= (data/node-type nested-condition) :value)
                             (= (data/node-value nested-condition) :else))]
    (if nested-is-else? branch-num (inc branch-num))))

(defn find-matching-branch-recursive
  "Recursively traverse branch structure to find which branch produces the result."
  [node suffix branch-num]
  (let [truthy-node (data/branch-truthy node)
        falsey-node (data/branch-falsey node)
        current-condition (data/branch-condition node)
        is-else-branch? (and current-condition
                             (= (data/node-type current-condition) :value)
                             (= (data/node-value current-condition) :else))

        ;; Check if current branch's truthy result matches our target
        truthy-match? (check-truthy-match truthy-node suffix is-else-branch?)

        ;; If current branch matches, return its number
        current-result (when truthy-match?
                         (if is-else-branch? "else" branch-num))

        ;; Check nested branches (for multi-condition cond)
        nested-result (when (and falsey-node
                                 (= (data/node-type falsey-node) :branch)
                                 (not current-result))
                        (let [nested-condition (data/branch-condition falsey-node)
                              nested-branch-num (determine-nested-branch-number nested-condition branch-num)]
                          (find-matching-branch-recursive falsey-node suffix nested-branch-num)))]

    (or current-result nested-result)))

(defn find-result-branch-number
  "Find which branch number a result edge belongs to by analyzing the embedded node name.
   For cond branches, determines which condition/result pair the embedded node represents."
  [from-node-def to-name from-name]
  (let [suffix (extract-node-name-suffix to-name from-name)
        is-else? (is-else-result? suffix)]
    (cond
      is-else? "else"
      suffix (find-matching-branch-recursive from-node-def suffix 1)
      :else nil)))

(defn find-cond-branch-label
  "Find the appropriate label for a dependency in a >cond branch structure.
   >cond creates nested >if structures with :else markers."
  [node from]
  (let [find-cond-label
        (fn find-label [node branch-num]
          (let [current-condition (data/branch-condition node)
                condition-inputs (if-let [condition (data/branch-condition node)] (data/node-inputs condition) #{})
                truthy-inputs (if-let [truthy (data/branch-truthy node)] (data/node-inputs truthy) #{})
                falsey-node (data/branch-falsey node)
                falsey-inputs (if-let [falsey (data/branch-falsey node)] (data/node-inputs falsey) #{})
                is-else-branch? (and current-condition
                                     (= (data/node-type current-condition) :value)
                                     (= (data/node-value current-condition) :else))]
            (cond
              (contains? condition-inputs from)
              (if is-else-branch? "else" "cond-expr")

              (contains? truthy-inputs from)
              (if is-else-branch? "else" "cond-expr")

              (and falsey-node (= (data/node-type falsey-node) :branch))
              (find-label falsey-node (inc branch-num))

              (contains? falsey-inputs from)
              "else"

              :else nil)))]
    (find-cond-label node 1)))

(defn find-or-branch-label
  "Find the appropriate label for a dependency in an >or branch structure.
   >or evaluates left-to-right and returns the first truthy value.
   Structure: condition=truthy (first option), falsey contains next option."
  [node from]
  (let [find-or-label
        (fn find-label [node branch-num]
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
              (and falsey-node (= (data/node-type falsey-node) :branch))
              (find-label falsey-node (inc branch-num))

              ;; If falsey is a value (final fallback), check if it matches
              (contains? falsey-inputs from)
              "or"

              :else nil)))]
    (find-or-label node 1)))

(defn find-and-branch-label
  "Find the appropriate label for a dependency in an >and branch structure.
   >and evaluates left-to-right and returns the first falsy value, or the last value if all are truthy.
   Structure: condition=falsey (current check), truthy contains next condition."
  [node from]
  (let [find-and-label
        (fn find-label [node branch-num]
          (let [condition-inputs (if-let [condition (data/branch-condition node)] (data/node-inputs condition) #{})
                truthy-node (data/branch-truthy node)
                truthy-inputs (if-let [truthy (data/branch-truthy node)] (data/node-inputs truthy) #{})
                falsey-inputs (if-let [falsey (data/branch-falsey node)] (data/node-inputs falsey) #{})

                ;; Also check nested dependencies in truthy branch
                all-truthy-deps (if truthy-node (graph/extract-dependencies-from-node truthy-node) #{})]
            (cond
              ;; Check if dependency is in the current condition
              (contains? condition-inputs from)
              "and"

              ;; Check if dependency is in the falsey (same as condition for >and)
              (contains? falsey-inputs from)
              "and"

              ;; Check if dependency is anywhere in the truthy branch (including nested)
              (contains? all-truthy-deps from)
              (if (and truthy-node (= (data/node-type truthy-node) :branch))
                ;; If truthy is a branch, recurse to get the proper nested label
                (let [recursive-result (find-label truthy-node (inc branch-num))]
                  (or recursive-result "and"))
                ;; If truthy is a leaf, it's the next condition
                "and")

              ;; If truthy is a leaf (final result), check if it matches directly
              (contains? truthy-inputs from)
              "and"

              :else nil)))]
    (find-and-label node 1)))

(defn html-escape
  "HTML escape function for function names in labels."
  [s]
  (when s
    (-> s
        (str/replace "&" "&amp;")
        (str/replace "<" "&lt;")
        (str/replace ">" "&gt;")
        (str/replace "\"" "&quot;"))))

(defn create-value-preview
  "Create a preview string for values, truncated to configured length."
  [value]
  (let [value-str (cond
                    (nil? value) "nil"
                    (string? value) (str "\"" value "\"")
                    (keyword? value) (str value)
                    (symbol? value) (str value)
                    (number? value) (str value)
                    (boolean? value) (str value)
                    (coll? value) (cond
                                    (empty? value) (if (map? value) "{}" "[]")
                                    (map? value) (str "{...} (" (count value) " items)")
                                    (vector? value) (str "[...] (" (count value) " items)")
                                    (set? value) (str "#{...} (" (count value) " items)")
                                    (seq? value) (str "(...) (" (count value) " items)")
                                    :else (str "coll (" (count value) " items)"))
                    :else (str value))
        truncated (truncate-if-long value-str DEFAULT-PREVIEW-TRUNCATION-LENGTH)]
    (html-escape (str "`" truncated "`"))))

(defn create-node-value-getter
  "Create a function to get value for any node (top-level or embedded)."
  [flow-env]
  (let [embedded-values (when flow-env
                          (apply merge
                                 (for [[node-id node-def] flow-env
                                       :when (= (data/node-type node-def) :branch)]
                                   (extract-embedded-nodes node-def node-id))))]
    (fn [node-id]
      (let [top-level-value (get-in flow-env [node-id ::data/value])
            embedded-value (get-in embedded-values [node-id ::data/value])]
        (cond
          (contains? (get flow-env node-id {}) ::data/value) top-level-value
          (contains? (get embedded-values node-id {}) ::data/value) embedded-value
          :else nil)))))

(defn create-branch-color-map
  "Create color mapping for branch nodes."
  [nodes]
  (let [color-fill-map {"coral" "lightcoral"
                        "skyblue" "lightskyblue"
                        "mediumpurple" "plum"
                        "orange" "peachpuff"
                        "forestgreen" "lightgreen"
                        "chocolate" "burlywood"
                        "hotpink" "lightpink"
                        "slategray" "lightsteelblue"
                        "steelblue" "powderblue"
                        "indianred" "mistyrose"
                        "darkcyan" "paleturquoise"
                        "goldenrod" "khaki"
                        "mediumseagreen" "honeydew"
                        "orchid" "lavender"
                        "cadetblue" "lightcyan"
                        "rosybrown" "lavenderblush"}
        available-colors (vec (keys color-fill-map))
        branch-nodes (filter #(= (:type (second %)) :branch) nodes)]
    {:color-fill-map color-fill-map
     :branch-color-assignments (into {} (map-indexed (fn [idx [branch-id _]]
                                                       [branch-id {:color (nth available-colors (mod idx (count available-colors)))}])
                                                     branch-nodes))}))

(defn create-display-label
  "Create display label for nodes based on type and embedding status."
  [node-type node-name is-embedded? primary-data]
  (if is-embedded?
    (if primary-data
      (str "<I>" (name node-type) "</I><BR/>" primary-data)
      (str "<I>" (name node-type) "</I>"))
    (if primary-data
      (str "<I>" (name node-type) "</I><BR/><U>" node-name "</U><BR/>" primary-data)
      (str "<I>" (name node-type) "</I><BR/><U>" node-name "</U>"))))

(defn create-branch-dot-attributes
  "Create DOT attributes for branch nodes."
  [node-id branch-color-assignments color-fill-map]
  (let [branch-info (get branch-color-assignments node-id)
        color (:color branch-info)
        fill-color (get color-fill-map color)]
    {:color color
     :fill-color fill-color}))

(defn generate-dot-node
  "Generate DOT format for a node with specified attributes."
  [node-name label-html shape style fillcolor & [color]]
  (let [color-attr (if color (str " color=" color) "")]
    (format "  \"%s\" [label=<%s> shape=%s style=\"%s\" fillcolor=%s%s];"
            node-name label-html shape style fillcolor color-attr)))

(defn create-value-display-label
  "Create display label for value nodes."
  [node-name is-embedded-value? preview-str]
  (create-display-label :value node-name is-embedded-value? preview-str))

(defn create-leaf-display-label
  "Create display label for leaf nodes."
  [node-name is-embedded-leaf? escaped-fn-name]
  (let [primary-data (when escaped-fn-name
                       (if (= escaped-fn-name "&lt;embedded&gt;")
                         "&lt;embedded&gt;"
                         (str "fn: " escaped-fn-name)))]
    (create-display-label :leaf node-name is-embedded-leaf? primary-data)))

(defn generate-branch-node-dot
  "Generate DOT format for branch nodes."
  [node-name node-id branch-color-assignments color-fill-map]
  (let [{:keys [fill-color]} (create-branch-dot-attributes node-id branch-color-assignments color-fill-map)
        label-html (format "<I>control flow</I><BR/><U>%s</U>" node-name)]
    (generate-dot-node node-name label-html "diamond" "filled" fill-color)))

(defn generate-value-node-dot
  "Generate DOT format for value nodes."
  [node-name node-id get-node-value]
  (let [is-embedded-value? (graph/is-embedded-node? node-name)
        actual-value (get-node-value node-id)
        preview-str (create-value-preview actual-value)
        display-label (create-value-display-label node-name is-embedded-value? preview-str)]
    (generate-dot-node node-name display-label "box" "filled,rounded" "aliceblue")))

(defn generate-leaf-node-dot
  "Generate DOT format for leaf nodes."
  [node-name node-id leaf-fn-names]
  (let [is-embedded-leaf? (graph/is-embedded-node? node-name)
        fn-name (or (get leaf-fn-names node-id)
                    (when is-embedded-leaf? "<embedded>"))
        escaped-fn-name (html-escape fn-name)
        display-label (create-leaf-display-label node-name is-embedded-leaf? escaped-fn-name)]
    (generate-dot-node node-name display-label "box" "filled,rounded" "honeydew")))

(defn generate-sequence-node-dot
  "Generate DOT format for sequence nodes."
  [node-name]
  (let [label-html (format "<I>sequence</I><BR/><U>%s</U>" node-name)]
    (generate-dot-node node-name label-html "ellipse" "filled,rounded" "lightcyan")))

(defn generate-unresolved-node-dot
  "Generate DOT format for unresolved nodes."
  [node-name]
  (let [label-html (format "<I>?unresolved</I><BR/><U>%s</U>" node-name)]
    (generate-dot-node node-name label-html "box" "rounded" "white" "red")))

(defn generate-default-node-dot
  "Generate DOT format for default/unknown node types."
  [node-name node-type]
  (let [label-html (format "<I>%s</I><BR/><U>%s</U>" (name node-type) node-name)]
    (generate-dot-node node-name label-html "box" "filled,rounded" "seashell")))

(defn generate-node-line
  "Generate a single DOT format line for a node."
  [node-id node-info leaf-fn-names get-node-value color-fill-map branch-color-assignments]
  (let [node-type (:type node-info)
        node-name (name node-id)]
    (case node-type
      :branch (generate-branch-node-dot node-name node-id branch-color-assignments color-fill-map)
      :value (generate-value-node-dot node-name node-id get-node-value)
      :leaf (generate-leaf-node-dot node-name node-id leaf-fn-names)
      :sequence (generate-sequence-node-dot node-name)
      :unresolved (generate-unresolved-node-dot node-name)
      ;; default
      (generate-default-node-dot node-name node-type))))

(defn generate-node-lines
  "Generate DOT format lines for all nodes."
  [nodes leaf-fn-names get-node-value color-fill-map branch-color-assignments]
  (for [[node-id node-info] nodes]
    (generate-node-line node-id node-info leaf-fn-names get-node-value color-fill-map branch-color-assignments)))

(defn determine-embedded-result-edge-label
  "Determine the label for an embedded result edge."
  [from-name to-name to-node-def]
  (let [edge-label (cond
                     (str/includes? from-name (str to-name "-truthy")) "truthy expr"
                     (str/includes? from-name (str to-name "-falsey")) "falsey expr"
                     (str/includes? from-name (str to-name "-else")) "else expr"
                     :else nil)]
    (if (and to-node-def (:cond-else? (graph/detect-branch-patterns to-node-def)))
      ;; For cond branches, convert to numbered cond-expr
      (let [branch-num (find-result-branch-number to-node-def from-name to-name)]
        (cond
          (= branch-num "else") "else expr"
          (number? branch-num) (str "cond-expr " branch-num)
          :else edge-label))
      ;; For regular if branches, keep the standard labels
      edge-label)))

(defn collect-deps-recursive
  "Recursively collect dependencies from nested branch structure using extraction function."
  [node extract-deps-fn]
  (if (= (data/node-type node) :branch)
    (let [current-deps (extract-deps-fn node)
          falsey-node (data/branch-falsey node)
          nested-deps (if (and falsey-node (= (data/node-type falsey-node) :branch))
                        (collect-deps-recursive falsey-node extract-deps-fn)
                        #{})]
      (clojure.set/union current-deps nested-deps))
    #{}))

(defn collect-condition-deps-recursive
  "Recursively collect all condition dependencies from nested branch structure."
  [node]
  (collect-deps-recursive node
                          (fn [node] (if-let [condition (data/branch-condition node)] (data/node-inputs condition) #{}))))

(defn collect-result-deps-recursive
  "Recursively collect all result expression dependencies from nested branch structure."
  [node]
  (collect-deps-recursive node
                          (fn [node]
                            (let [truthy-deps (if-let [truthy (data/branch-truthy node)] (data/node-inputs truthy) #{})
                                  falsey-deps (if-let [falsey (data/branch-falsey node)] (data/node-inputs falsey) #{})]
                              (clojure.set/union truthy-deps falsey-deps)))))

(defn extract-direct-branch-deps
  "Extract direct dependencies from branch node parts."
  [to-node-def]
  (let [condition-deps (if-let [condition (data/branch-condition to-node-def)] (data/node-inputs condition) #{})
        truthy-deps (if-let [truthy (data/branch-truthy to-node-def)] (data/node-inputs truthy) #{})
        falsey-deps (if-let [falsey (data/branch-falsey to-node-def)] (data/node-inputs falsey) #{})]
    {:condition-deps condition-deps
     :truthy-deps truthy-deps
     :falsey-deps falsey-deps}))

(defn collect-all-nested-deps
  "Collect all nested dependencies for cond branches."
  [to-node-def]
  (let [all-condition-deps (collect-condition-deps-recursive to-node-def)
        all-result-deps (collect-result-deps-recursive to-node-def)]
    {:all-condition-deps all-condition-deps
     :all-result-deps all-result-deps}))

(defn collect-dependency-sets
  "Collect various dependency sets for a branch node."
  [to-node-def]
  (let [nested-deps (collect-all-nested-deps to-node-def)
        direct-deps (extract-direct-branch-deps to-node-def)]
    (merge nested-deps direct-deps)))

(defn determine-multiple-cond-edges
  "Determine if a cond branch dependency needs multiple edges."
  [is-cond-else? all-condition-deps all-result-deps from to-node-def]
  (when is-cond-else?
    (let [is-condition? (contains? all-condition-deps from)
          is-result? (contains? all-result-deps from)
          branch-numbers (graph/find-cond-branch-numbers to-node-def from)]
      (cond
        (and is-condition? is-result?)
        ;; Create both cond-test and cond-expr edges with branch numbers
        (concat
         (for [branch-num branch-numbers]
           [(str "cond-test " branch-num) ""])
         (for [branch-num branch-numbers]
           [(str "cond-expr " branch-num) " style=dashed"]))

        is-condition?
        (for [branch-num branch-numbers]
          [(str "cond-test " branch-num) ""])

        is-result?
        (for [branch-num branch-numbers]
          [(str "cond-expr " branch-num) " style=dashed"])

        :else
        nil))))

(defn determine-single-branch-label
  "Determine the label for a single branch edge."
  [is-or-pattern? is-and-pattern? is-cond-else? multiple-edges condition-deps truthy-deps falsey-deps
   all-condition-deps all-result-deps from to-node-def branch-label-result]
  (cond
    ;; For >or and >and patterns, use special logic
    is-or-pattern? (cond
                     (contains? condition-deps from) (or branch-label-result "or")
                     (contains? truthy-deps from) (or branch-label-result "or")
                     (contains? falsey-deps from) (or branch-label-result "or")
                     branch-label-result branch-label-result
                     :else "or")

    is-and-pattern? (or branch-label-result "and")

    ;; For >cond patterns, use mixed logic (only when not multiple edges)
    (and is-cond-else? (not multiple-edges))
    (let [branch-numbers (graph/find-cond-branch-numbers to-node-def from)
          branch-num (first branch-numbers)]
      (cond
        (contains? all-condition-deps from) (if (= branch-num "else") "else" (str "cond-test " branch-num))
        (contains? all-result-deps from) (if (= branch-num "else") "else" (str "cond-expr " branch-num))
        (contains? truthy-deps from) (if (= branch-num "else") "else" (str "cond-expr " branch-num))
        (contains? falsey-deps from) (if (= branch-num "else") "else" (str "cond-expr " branch-num))
        branch-label-result branch-label-result
        :else (or branch-label-result "cond-expr")))

    ;; For simple >if patterns, use basic labels
    :else (cond
            (contains? condition-deps from) "condition"
            (contains? truthy-deps from) "truthy"
            (contains? falsey-deps from) "falsey"
            :else "control flow")))

(defn is-embedded-result-edge?
  "Check if this is an embedded result edge."
  [from-name to-name]
  (or (str/includes? from-name (str to-name "-truthy"))
      (str/includes? from-name (str to-name "-falsey"))
      (str/includes? from-name (str to-name "-else"))))

(defn generate-embedded-result-edge-dot
  "Generate DOT format for embedded result edges."
  [from-name to-name branch-color to-node-def]
  (let [final-edge-label (determine-embedded-result-edge-label from-name to-name to-node-def)]
    (format "  \"%s\" -> \"%s\" [color=%s style=\"bold,dashed\" label=\"%s\" penwidth=3];"
            from-name to-name branch-color final-edge-label)))

(defn create-multiple-cond-edges-dot
  "Create DOT format for multiple cond edges."
  [multiple-edges from to branch-color]
  (str/join "\n" (for [[label style-attrs] multiple-edges]
                   (format "  \"%s\" -> \"%s\" [color=%s%s label=\"%s\" penwidth=2];"
                           (name from) (name to) branch-color style-attrs label))))

(defn determine-edge-style-attributes
  "Determine style attributes for branch edges."
  [branch-label]
  (if (or (= branch-label "cond-expr")
          (= branch-label "truthy")
          (= branch-label "falsey")
          (= branch-label "and")
          (= branch-label "or"))
    " style=dashed"
    ""))

(defn generate-single-branch-edge-dot
  "Generate DOT format for single branch edges."
  [from to branch-color branch-label]
  (let [style-attrs (determine-edge-style-attributes branch-label)]
    (format "  \"%s\" -> \"%s\" [color=%s%s label=\"%s\" penwidth=2];"
            (name from) (name to) branch-color style-attrs branch-label)))

(defn extract-branch-edge-context
  "Extract context needed for branch edge generation."
  [to-node-def from]
  (let [dep-sets (collect-dependency-sets to-node-def)
        {:keys [all-condition-deps all-result-deps]} dep-sets
        {:keys [cond-else? and? or?]} (graph/detect-branch-patterns to-node-def)
        branch-label-result (cond
                              cond-else? (find-cond-branch-label to-node-def from)
                              or? (find-or-branch-label to-node-def from)
                              and? (find-and-branch-label to-node-def from)
                              :else nil)
        multiple-edges (determine-multiple-cond-edges cond-else? all-condition-deps all-result-deps from to-node-def)]
    {:dep-sets dep-sets
     :patterns {:cond-else? cond-else? :or? or? :and? and?}
     :branch-label-result branch-label-result
     :multiple-edges multiple-edges}))

(defn generate-regular-branch-edge-dot
  "Generate DOT format for regular dependency edges into branches."
  [from to to-node-def branch-color]
  (let [{:keys [dep-sets patterns branch-label-result multiple-edges]} (extract-branch-edge-context to-node-def from)]
    (if multiple-edges
      (create-multiple-cond-edges-dot multiple-edges from to branch-color)
      (let [{:keys [cond-else? or? and?]} patterns
            branch-label (determine-single-branch-label
                          or? and? cond-else? multiple-edges
                          (:condition-deps dep-sets) (:truthy-deps dep-sets) (:falsey-deps dep-sets)
                          (:all-condition-deps dep-sets) (:all-result-deps dep-sets)
                          from to-node-def branch-label-result)]
        (generate-single-branch-edge-dot from to branch-color branch-label)))))

(defn generate-branch-edge-dot
  "Generate DOT format for edges targeting branch nodes."
  [from to flow-env branch-color-assignments]
  (let [branch-info (get branch-color-assignments to)
        branch-color (:color branch-info)
        to-node-def (get (or flow-env {}) to)
        from-name (name from)
        to-name (name to)]
    (if (is-embedded-result-edge? from-name to-name)
      (generate-embedded-result-edge-dot from-name to-name branch-color to-node-def)
      (generate-regular-branch-edge-dot from to to-node-def branch-color))))

(defn generate-sequence-edge-dot
  "Generate DOT format for sequence edges."
  [from to from-node-info to-node-info]
  (cond
    ;; If target is a sequence, use special styling to indicate mapping operation
    (= (:type to-node-info) :sequence)
    (format "  \"%s\" -> \"%s\" [color=purple label=\"map\" style=bold penwidth=2];" (name from) (name to))

    ;; If source is a sequence, use special styling to indicate sequential output
    (= (:type from-node-info) :sequence)
    (format "  \"%s\" -> \"%s\" [color=purple style=dashed];" (name from) (name to))

    :else nil))

(defn generate-regular-edge-dot
  "Generate DOT format for regular dependency edges."
  [from to]
  (format "  \"%s\" -> \"%s\" [color=black];" (name from) (name to)))

(defn generate-edge-line
  "Generate a single DOT format line for an edge."
  [from to nodes flow-env branch-color-assignments]
  (let [to-node-info (get nodes to)
        from-node-info (get nodes from)]
    (cond
      ;; If target is a branch, determine which part of the branch this input feeds
      (= (:type to-node-info) :branch)
      (generate-branch-edge-dot from to flow-env branch-color-assignments)

      ;; Handle sequence edges (both directions)
      (or (= (:type to-node-info) :sequence)
          (= (:type from-node-info) :sequence))
      (generate-sequence-edge-dot from to from-node-info to-node-info)

      ;; Regular dependencies
      :else
      (generate-regular-edge-dot from to))))

(defn generate-edge-lines
  "Generate DOT format lines for all edges."
  [edges nodes flow-env branch-color-assignments]
  (for [[from to] edges]
    (generate-edge-line from to nodes flow-env branch-color-assignments)))

(defn graph-to-dot
  "Convert graph structure to DOT format for Graphviz with vertical bias."
  [{:keys [nodes edges]} & [flow-env]]
  (let [;; Create node value getter function
        get-node-value (create-node-value-getter flow-env)

        ;; Create color mappings for branch nodes
        color-config (create-branch-color-map nodes)
        color-fill-map (:color-fill-map color-config)
        branch-color-assignments (:branch-color-assignments color-config)]

    (str "digraph NodelyGraph {\n"
         "  // Layout directives for vertical bias\n"
         "  rankdir=TB;\n"
         "  ranksep=1.2;\n" ; Increase vertical separation between ranks
         "  nodesep=0.8;\n" ; Reduce horizontal separation between nodes
         "  concentrate=true;\n" ; Merge parallel edges to reduce horizontal spread
         "  \n"
         "  // Node and edge styling\n"
         "  node [fontname=\"Arial\"];\n"
         "  edge [fontname=\"Arial\" fontsize=10];\n"
         "  \n"
         "  // Node definitions\n"
         (str/join "\n" (generate-node-lines nodes {} get-node-value color-fill-map branch-color-assignments)) "\n"
         "  \n"
         "  // Edge definitions\n"
         (str/join "\n" (generate-edge-lines edges nodes flow-env branch-color-assignments)) "\n"
         "}")))

(defn analyze-nodely-env
  "Comprehensive analysis of a nodely environment with visualization.

   Usage:
   - (analyze-nodely-env env) - generates analysis and DOT visualization"
  [env]
  (let [;; Use the base analysis from graph namespace
        base-analysis (graph/analyze-nodely-env env)

        ;; Get graph structure with embedded nodes for visualization
        graph-structure (graph/extract-graph-structure env :include-embedded-nodes? true)]

    ;; Extend base analysis with visualization-specific features
    (assoc base-analysis
           :dot-format (graph-to-dot graph-structure env))))
