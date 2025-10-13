(ns nodely.data
  (:refer-clojure :exclude [map sequence flatten])
  (:require
   [clojure.set :as set]
   [schema.core :as s])
  (:import
   [clojure.lang IFn]
   [java.util.concurrent CompletableFuture CompletionException]
   [java.util.function Function]))

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

(defn node-inputs
  ([node]
   (node-inputs node #{}))
  ([node inputs]
   (case (::type node)
     :value    inputs
     :leaf     (set/union inputs (::inputs node))
     :branch   (recur (::condition node) inputs)
     :sequence (recur (::process-node node)
                      (conj inputs (::input node))))))

(defn catching
  "Like `clojure.core/comp`, except only affects throw completions of
  the wrapped `g`. `f` must be a function of a single Exception, and
  will be passed the exception thrown from `g` if `g` completes
  exceptionally."
  [f g]
  (fn [& args]
    (try (apply g args)
         (catch Throwable t (f t)))))

(defn update-leaf
  [leaf f compositor]
  ;; f new function, arg is old function
  (update leaf :nodely.data/fn #(compositor f %)))

(declare env-update-helper)

(defn update-branch
  [{::keys [condition truthy falsey]}
   f
   {:keys [apply-to-condition?]
    :or   {apply-to-condition? false} :as opts}
   compositor]
  #::{:type      :branch
      :condition (if apply-to-condition?
                   (env-update-helper condition f opts compositor)
                   condition)
      :falsey    (env-update-helper falsey f opts compositor)
      :truthy    (env-update-helper truthy f opts compositor)})

(defn update-sequence
  [sequence f compositor]
  (update sequence ::process-node env-update-helper f {} compositor))

(defn env-update-helper
  [node f opts compositor]
  (case (::type node)
    :value    (update node ::value f)
    :leaf     (update-leaf node f compositor)
    :branch   (update-branch node f opts compositor)
    :sequence (update-sequence node f compositor)))

(defn update-node
  ([node f opts]
   (env-update-helper node f opts comp))
  ([node f]
   (update-node node f {})))

(defn catch-node
  ([node f opts]
   (env-update-helper node f opts catching))
  ([node f]
   (catch-node node f {})))

;;
;; Env Utils
;;

(defn with-error-handler
  [env handler]
  (update-vals env #(catch-node % handler {:apply-to-condition? true})))

#_(defn with-try-clause-expr
    [[_ t s expr]]
    (fn [error]
      (when (instance? error type)
        expr)))

;; '(with-try env (catch type symbol expr))
;;  (update-vals env (fn [error] (if (instance? error type)
;;                                   ()
;;  ))
#_(defn with-try-expr
    [env & clauses]
    (let [clauses (for [[catch t s expr] clauses]
                    (do (assert (= catch 'catch))
                        [t s expr]))]))

(defn tuple-to-handler
  [m]
  (fn [error]
    (if-let [f (some (fn [[ex-class handler]] (when (instance? ex-class error) handler)) m)]
      (f error)
      (throw error))))

(defn with-try-expr
  [clauses]
  (let [clauses (into [] (for [[c t s expr] clauses]
                           (do (assert (= c 'catch))
                               (if-let [t (resolve t)]
                                 [t (eval `(fn [~s] ~expr))]
                                 (throw (ex-info (str "Could not resolve exception class: " t) {:type t}))))))]
    clauses))

(defmacro with-try
  [env & body]
  `(with-error-handler
     ~env
     (tuple-to-handler ~(with-try-expr body))))

(comment
  (macroexpand-1 '(with-try {:a (leaf [:x] (comp inc :x))}
                    (catch Exception e (println e))
                    (catch Throwable t (println t))))

  (tuple-to-handler {java.lang.Exception identity,
                     java.lang.Throwable identity})

  ;
  )

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
