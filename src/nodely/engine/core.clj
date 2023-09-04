(ns nodely.engine.core
  (:refer-clojure :exclude [eval resolve])
  (:require
   [clojure.set :as set]
   [loom.alg :as alg]
   [loom.graph :as graph]
   [nodely.data :as data]))
;;
;; Graph Utilities
;;

(defn- nested-inputs
  "Internal use only"
  [node]
  (set/union (data/node-inputs node)
             (set (::data/inputs (meta node)))))

;; FIXME: Add a way for this fn to accept a function that returns the inputs of a node
(defn env->graph
  [env]
  (if (empty? env)
    (graph/->BasicEditableDigraph {} {} {})
    (let [input-mapping (reduce-kv (fn [acc k v]
                                     (assoc acc k (nested-inputs v))) {} env)]
      (graph/transpose (graph/digraph input-mapping)))))

(defn- edges-that-reach
  ([k graph] (edges-that-reach k graph #{}))
  ([k graph seen]
   (when (seen k)
     (throw (ex-info "Detected cycle computing edges-that-reach"
                     {:graph graph
                      :cycled-node (seen k)})))
   (let [predecessors (set (graph/predecessors graph k))
         seen (conj seen k)]
     (reduce #(into %1 (edges-that-reach %2 graph seen)) predecessors predecessors))))

(defn dependencies-for
  [k env]
  (let [g (env->graph env)]
    (edges-that-reach k g)))

;;
;; Resolving nodes
;;

(declare resolve-branch)
(declare resolve-leaf)
(declare resolve-sequence)
(declare branch->value)
(declare leaf->value)
(declare sequence->value)

(defn node->value
  [node env]
  (case (::data/type node)
    :value    [node env]
    :leaf     (leaf->value node env)
    :branch   (branch->value node env)
    :sequence (sequence->value node env)))

(defn resolve
  [k env]
  (let [[node new-env] (node->value (get env k) env)]
    (assoc new-env k node)))

(defn- resolve-inputs
  [in env]
  (reduce (fn [env k] (resolve k env)) env in))

(defn prepare-inputs
  [input-keys env]
  (->> (select-keys env input-keys)
       (map (juxt key (comp ::data/value val)))
       (into {})))

(defn eval-leaf
  [node in]
  (let [f (::data/fn node)]
    (data/value (f in))))

(defn leaf+env-with-values->value
  [leaf env-with-values]
  (let [in (prepare-inputs (::data/inputs leaf) env-with-values)]
    (eval-leaf leaf in)))

(defn leaf->value
  [node env]
  (let [new-env (resolve-inputs (::data/inputs node) env)]
    [(leaf+env-with-values->value node new-env) new-env]))

(defn resolve-leaf
  [k env]
  (let [[node new-env] (leaf->value (get env k) env)]
    (assoc new-env k node)))

(defn branch-step
  "Evaluates a branch node once"
  [node env]
  (let [condition-node (::data/condition node)
        [condition-value new-env] (node->value condition-node env)]
    (if (::data/value condition-value)
      [(::data/truthy node) new-env]
      [(::data/falsey node) new-env])))

(defn resolve-one-branch
  [k env]
  (let [[node new-env] (branch-step (get env k) env)]
    (assoc new-env k node)))

(defn branch->value
  [node env]
  (let [[new-node new-env] (branch-step node env)]
    (node->value new-node new-env)))

(defn- resolve-branch
  [k env]
  (resolve k (resolve-one-branch k env)))

(defn- sequence->value
  [node env]
  (let [in-key  (::data/input node)
        f       (::data/fn node)
        new-env (resolve-inputs [in-key] env)
        in      (data/get-value new-env in-key)]
    [(data/value (mapv f in)) new-env]))

(defn- resolve-sequence
  [k env]
  (let [[node new-env] (sequence->value (get env k) env)]
    (assoc new-env k node)))

(defn unbranch
  [k env]
  (let [node (get env k)]
    (case (::data/type node)
      :branch (let [new-env (resolve-one-branch k env)]
                (recur k new-env))
      env)))

(defn unbranch-all
  [env]
  (reduce-kv (fn [env k v]
               (case (::data/type v)
                 :branch (unbranch k env)
                 env)) env env))

;;
;; Branch Paths
;;

(defn branch?
  [node]
  (= :branch (::data/type node)))

(defn resolve-transitive-dependencies
  [env deps]
  (apply set/union deps
         (map #(dependencies-for % env) deps)))

(defn- confirm-satisfaction
  ;; Receives an open-candidate with mixed :analyzed and :satisfied
  ;; nodelists.
  ;;
  ;; Returns an open-candidate with only satisfied nodes, OR, throws
  ;; ex-info if it cannot validate that all analyzed nodes can become
  ;; satisfied by:
  ;;
  ;; - Finding all dependencies in satisfied
  ;;
  ;; - Satisfying peer analyzed nodes first, then satisfying the node
  ;;   depending on analyzed nodes.
  [{:keys [env] :as subject}]
  (loop [analyzed (:analyzed subject)
         satisfied (:satisfied subject)]
    (if (empty? analyzed)
      (assoc subject
             :analyzed analyzed
             :satisfied satisfied)
      (let [satisfied-analyzed (set (filter #(let [deps (dependencies-for % env)]
                                               (= deps
                                                  (set/intersection satisfied deps))) analyzed))]
        (if (empty? satisfied-analyzed)
          (throw (ex-info "Can't anchor graph" {::subject subject}))
          (recur (set/difference analyzed satisfied-analyzed)
                 (set/union satisfied satisfied-analyzed)))))))

(defn- identify-deps
  ;; When adding nodes to identified, we do not want to redundantly
  ;; re-examine an already analyzed or satisfied node in the
  ;; identified path. When adding one or more nodes to the identified
  ;; set, remove all nodes that have already been tracked in analyzed
  ;; or satisfied.
  [{:keys [satisfied analyzed] :as open-candidate} deps]
  (update open-candidate
          :identified into (set/difference (set deps)
                                           (set/union satisfied analyzed))))

(defn- expand-branches
  ;; Given an open-candidate and k, identifying a node which is a
  ;; branch, return a vector containing all new open candidates such that:
  ;;
  ;; - k no longer holds the branch described on input
  ;;
  ;; - the open-candidates represent the possible resolutions of conditions
  ;;   consequents of the branch
  ;;
  ;; - the returned open-candidates include the branch conditions'
  ;;   dependencies as metadata dependencies
  [open-candidate address]
  (let [cur-env                (:env open-candidate)
        {::data/keys [condition truthy falsey]
         :as         cur-node} (get-in cur-env address)]
    (cond
      (branch? condition)
      (let [condition-address (conj address ::data/condition)]
        (expand-branches open-candidate condition-address))

      :else
      (let [inputs (nested-inputs cur-node)]
        [(-> open-candidate
             (update :path conj [:truthy cur-node])
             (update :env assoc-in address (vary-meta truthy update ::data/inputs into inputs))
             (identify-deps inputs))
         (-> open-candidate
             (update :path conj [:falsey cur-node])
             (update :env assoc-in address (vary-meta falsey update ::data/inputs into inputs))
             (identify-deps inputs))]))))

(defn all-paths-for-node
  "Given a key `k` in `env`, finds all paths to evaluate `k` in `env` such that:

  - Unwrap all branches and only the branches of all dependencies to evaluate `k`

  - A path is a list of choices

  - A choice is a decision (either `truthy` or `falsey`) and a branch node that was unwrapped.

  - Returns a list of pair of `path` and `env`, each `path` is the
  collection of choices that yields a DAG to `k`"
  [k env]
  (loop [open   (list {:env env :path [] :identified #{k} :analyzed #{} :satisfied #{}})
         closed '()]
    (let [open-candidate (first open)]
      (cond
        (nil? open-candidate) (map (juxt :path :env) closed)
        (-> open-candidate
            :identified
            seq)              (let [cur-key  (-> open-candidate :identified first)
                                    cur-env  (:env open-candidate)
                                    cur-node (get cur-env cur-key)]
                                (if (branch? cur-node)
                                  (recur (concat (expand-branches open-candidate [cur-key])
                                                 (rest open))
                                         closed)
                                  (let [deps                  (dependencies-for cur-key cur-env)
                                        update-open-candidate (cond-> open-candidate
                                                                true          (update :identified disj cur-key)
                                                                (empty? deps) (update :satisfied conj cur-key)
                                                                (seq deps)    (-> (update :analyzed conj cur-key)
                                                                                  (identify-deps deps)))]
                                    (recur (conj (rest open) update-open-candidate)
                                           closed))))
        (-> open-candidate
            :analyzed
            seq)              (recur (rest open)
                                     (conj closed (confirm-satisfaction open-candidate)))
        (-> open-candidate
            :satisfied
            seq)              (recur (rest open)
                                     (conj closed open-candidate))))))

(defn node+path-dependencies
  [k [path env]]
  (let [path-nodes (map second path)
        path-deps  (set (mapcat #(get-in % [::data/condition ::data/inputs]) path-nodes))
        node-deps  (set (dependencies-for k env))]
    (set/union node-deps path-deps)))

(defn env+dependencies-for-outcome
  [k [path env]]
  (let [first-order-dependencies (node+path-dependencies k [path env])]
    [env (resolve-transitive-dependencies env first-order-dependencies)]))

(defn committed-dependencies
  "Returns the list of dependencies to evaluate `k` in `env` that must
  deterministically be evaluated in order to evaluate `k`. This
  accounts for branching, and if there is distinctness in what nodes
  must be evaluated in various branches to evaluate `k`, returns a
  list only including those nodes that must be evaluated in every
  case."
  [k env]
  (let [paths+envs    (all-paths-for-node k env)
        envs+deps     (map (partial env+dependencies-for-outcome k) paths+envs)
        sorted-keys   (map (comp set alg/topsort env->graph (partial apply select-keys)) envs+deps)
        commited-deps (apply set/intersection sorted-keys)]
    (if (and (branch? (get env k))
             (every? (partial contains? commited-deps) (dependencies-for k env)))
      (conj commited-deps k)
      commited-deps)))

;;
;; Detecting Cycles
;;

(defn check-env
  "Returns nil if there is no cycle. Returns a map with useful information if there is cycle"
  ([k env]
   (try (all-paths-for-node k env)
        nil
        (catch clojure.lang.ExceptionInfo e
          (ex-data e))))
  ([env]
   (some identity
         (for [k (keys env)]
           (check-env k env)))))

(defn checked-env
  "Checks env, raising an exception if checks fail."
  [env]
  (if (check-env env)
    (throw (ex-info "Checked-env found cycles at compile time" {:env env}))
    env))
