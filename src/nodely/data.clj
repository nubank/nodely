(ns nodely.data
  (:refer-clojure :exclude [map sequence flatten])
  (:require
   [clojure.set :as set]
   [schema.core :as s]))

;;
;; Node Definitions
;;

(declare Node)

(def node-tag (s/enum ::blocking))

(s/defschema Value #::{:type  (s/eq :value)
                       :value s/Any})

(s/defschema Leaf #::{:type   (s/eq :leaf)
                      :inputs #{s/Keyword}
                      :fn     (s/pred ifn?)
                      :tags   #{node-tag}})

(s/defschema Branch #::{:type      (s/eq :branch)
                        :condition (s/recursive #'Node)
                        :truthy    (s/recursive #'Node)
                        :falsey    (s/recursive #'Node)})

(s/defschema Sequence #::{:type         (s/eq :sequence)
                          :input        s/Keyword
                          :process-node (s/conditional
                                         #(= (get % ::type) :value) Value
                                         #(= (get % ::type) :leaf) Leaf)
                          :tags         #{node-tag}})

(s/defschema Node (s/conditional
                   #(= (get % ::type) :value) Value
                   #(= (get % ::type) :leaf) Leaf
                   #(= (get % ::type) :branch) Branch
                   #(= (get % ::type) :sequence) Sequence))

(s/defschema Env {s/Keyword Node})

;;
;; Node Constructors
;;

(s/defn value :- Value
  [v :- s/Any]
  #::{:type  :value
      :value v})

(s/defn leaf :- Leaf
  ([inputs :- (s/pred seqable?)
    fn]
   (leaf inputs fn #{}))
  ([inputs :- (s/pred seqable?)
    fn
    tags :- #{node-tag}]
   #::{:type   :leaf
       :inputs (set inputs)
       :fn     fn
       :tags   tags}))

(s/defn branch :- Branch
  [condition-node :- Node
   truthy-node :- Node
   falsey-node :- Node]
  #::{:type      :branch
      :condition condition-node
      :truthy    truthy-node
      :falsey    falsey-node})

(s/defn sequence
  ([input :- s/Keyword
    f]
   (sequence input f #{}))
  ([input :- s/Keyword
    f
    tags :- #{node-tag}]
   #::{:type         :sequence
       :input        input
       :process-node (value f)
       :tags         tags})
  ([input :- s/Keyword
    closure-inputs
    f
    tags :- #{node-tag}]
   #::{:type         :sequence
       :input        input
       :process-node (leaf closure-inputs f)
       :tags         tags}))

;;
;; Node Utilities
;;

(defn node? [n]
  (boolean (::type n)))

(defn value?
  [node]
  (= :value (::type node)))

(defn leaf?
  [node]
  (= :leaf (::type node)))

(defn branch?
  "Check if a node is a branch node."
  [node]
  (= :branch (::type node)))

(defn sequence?
  "Check if a node is a sequence node."
  [node]
  (= :sequence (::type node)))

(defn else-condition?
  "Check if a condition represents an :else clause."
  [condition]
  (and condition
       (value? condition)
       (= (::value condition) :else)))

(defn node-type
  "Get the type of a node."
  [node]
  (::type node))

(defn node-value
  "Get the value from a value node."
  [node]
  (when (value? node)
    (::value node)))

(defn branch-condition
  "Get the condition from a branch node."
  [node]
  (when (branch? node)
    (::condition node)))

(defn branch-truthy
  "Get the truthy branch from a branch node."
  [node]
  (when (branch? node)
    (::truthy node)))

(defn branch-falsey
  "Get the falsey branch from a branch node."
  [node]
  (when (branch? node)
    (::falsey node)))

(defn sequence-input
  "Get the input from a sequence node."
  [node]
  (when (sequence? node)
    (::input node)))

(defn node-inputs
  "Get the inputs from a node for execution dependency resolution.
   For branches, only follows the condition path (execution dependencies)."
  ([node]
   (node-inputs node #{}))
  ([node inputs]
   (case (::type node)
     :value    inputs
     :leaf     (set/union inputs (::inputs node))
     :branch   (recur (::condition node) inputs)
     :sequence (recur (::process-node node)
                      (conj inputs (::input node))))))

(defn node-all-inputs
  "Get ALL possible inputs from a node for static analysis.
   Unlike node-inputs, for branches this collects from condition, truthy, AND falsey paths.
   This is useful for graph analysis where you need to know all possible dependencies."
  ([node]
   (node-all-inputs node #{}))
  ([node inputs]
   (case (::type node)
     :value    inputs
     :leaf     (set/union inputs (::inputs node))
     :branch   (let [condition (::condition node)
                     truthy (::truthy node)
                     falsey (::falsey node)]
                 (set/union
                  (if condition (node-all-inputs condition #{}) #{})
                  (if truthy (node-all-inputs truthy #{}) #{})
                  (if falsey (node-all-inputs falsey #{}) #{})))
     :sequence (recur (::process-node node)
                      (conj inputs (::input node))))))

;;
;; Env Utils
;;

(s/defn get-value :- s/Any
  [env :- Env
   k :- s/Keyword]
  (let [node (get env k)]
    (if (value? node)
      (::value node)
      (throw (ex-info "Node type is not a value" {:node node})))))

(s/defn values->env :- Env
  [m :- {s/Keyword s/Any}]
  (reduce-kv (fn [env k v] (assoc env k (value v))) {} m))

(s/defn merge-values :- Env
  [env :- Env
   values :- {s/Keyword s/Any}]
  (merge env (values->env values)))

(s/defn flatten :- [Node]
  [node :- Node]
  (case (::type node)
    :branch (cons node (concat (flatten (::condition node)) (flatten (::truthy node)) (flatten (::falsey node))))
    [node]))

(defn branch-count
  [env]
  (count (filter #(= (::type %) :branch) (mapcat flatten (vals env)))))
