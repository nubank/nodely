(ns nodely.data
  (:refer-clojure :exclude [map sequence])
  (:require [clojure.set :as set]
            [schema.core :as s]))

;;
;; Node Definitions
;;

(declare Node)

(s/defschema Value #::{:type  (s/eq :value)
                       :value s/Any})

(s/defschema Leaf #::{:type   (s/eq :leaf)
                      :inputs #{s/Keyword}
                      :fn     (s/pred ifn?)})

(s/defschema Branch #::{:type      (s/eq :branch)
                        :condition (s/recursive #'Node)
                        :truthy    (s/recursive #'Node)
                        :falsey    (s/recursive #'Node)})

(s/defschema Sequence #::{:type  (s/eq :sequence)
                          :input s/Keyword
                          :fn    (s/pred ifn?)})

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
  [inputs :- (s/pred seqable?)
   fn]
  #::{:type   :leaf
      :inputs (set inputs)
      :fn     fn})

(s/defn branch :- Branch
  [condition-node :- Node
   truthy-node :- Node
   falsey-node :- Node]
  #::{:type      :branch
      :condition condition-node
      :truthy    truthy-node
      :falsey    falsey-node})

(s/defn sequence
  [input :- s/Keyword
   fn]
  #::{:type  :sequence
      :input input
      :fn    fn})

;;
;; Node Utilities
;;

(defn node? [n]
  (boolean (::type n)))

(defn value?
  [node]
  (= :value (::type node)))

(defn node-inputs
  [node]
  (case (::type node)
    :value    #{}
    :leaf     (::inputs node)
    :branch   (recur (::condition node))
    :sequence #{(::input node)}))

(defn node-inputs-v2
  [node]
  (case (::type node)
    :value    #{}
    :leaf     (::inputs node)
    :branch   (let [condition-inputs (node-inputs-v2 (::condition node))
                    truthy-inputs    (node-inputs-v2 (::truthy node))
                    falsey-inputs    (node-inputs-v2 (::falsey node))]
                (set/union condition-inputs truthy-inputs falsey-inputs))
    :sequence #{(::input node)}))

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
