(ns nodely.data
  (:refer-clojure :exclude [map sequence])
  (:require
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

(s/defschema Sequence (s/either #::{:type  (s/eq :sequence)
                                    :input s/Keyword
                                    :fn    (s/pred ifn?)
                                    :tags  #{node-tag}}
                                #::{:type           (s/eq :sequence)
                                    :input          s/Keyword
                                    :closure-inputs [s/Keyword]
                                    :fn-fn          (s/pred ifn?)
                                    :tags  #{node-tag}}))

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
   #::{:type  :sequence
       :input input
       :fn    f
       :tags  tags})
  ([input :- s/Keyword
    closure-inputs
    f
    tags :- #{node-tag}]
   #::{:type           :sequence
       :input          input
       :closure-inputs closure-inputs
       :fn-fn          f
       :tags           tags}))

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
