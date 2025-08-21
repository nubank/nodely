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

;; Completablefuture based exception and return composition, need to enable try-env next
;; try-env needs
;; unwrap exceptions (always CompletionException thrown from CF wrapping actual exception)
;; determine if Exception type was specified to be caught (including inheritence, order of expression matters in try catch, can't use a map!)
;; Run body with caught exception and yield result

(defn- jfunctionify
  [^clojure.lang.IFn f]
  (reify Function (apply [_ o] (.invoke f o))))

(defprotocol ComposableInvokable
  (compose-return [ci f] "Returns a new ComposableInvokable that will invoke f on the return of cf, when it returns successfully.")
  (compose-throw [ci f] "Returns a new ComposableInvokable that will invoke f on the exception thrown from ci, when it ends exceptionally."))

(deftype CompletableFutureComposableInvokable [compositions]
  ComposableInvokable
  (compose-return [_ f] (CompletableFutureComposableInvokable. (conj compositions [:apply f])))
  (compose-throw [_ f] (CompletableFutureComposableInvokable. (conj compositions [:throw f])))
  IFn
  (invoke [_ arg]
    (loop [cf (CompletableFuture/completedFuture arg)
           [[op func] & rem] compositions]
      (if (nil? func)
        @cf
        (recur (case op
                 :apply (.thenApply cf (jfunctionify func))
                 :throw (.exceptionally cf (jfunctionify func)))
               rem)))))

(extend-protocol ComposableInvokable
  IFn
  (compose-return [fn f] (CompletableFutureComposableInvokable. (list [:apply fn] [:apply f])))
  (compose-throw [fn f] (CompletableFutureComposableInvokable. (list [:apply fn] [:throw f]))))

(defn update-leaf
  [leaf f]
  ;; f new function, arg is old function
  (update leaf :nodely.data/fn compose-return f))

(declare update-node)

(defn update-branch
  [{::keys [condition truthy falsey]}
   f
   {:keys [apply-to-condition?]
    :or   {apply-to-condition? false}}]
  #::{:type      :branch
      :condition (if apply-to-condition?
                   (update-node condition f)
                   condition)
      :falsey    (update-node falsey f)
      :truthy    (update-node truthy f)})

(defn update-sequence
  [sequence f]
  (update sequence ::process-node update-node f))

(defn update-node
  ([node f opts]
   (case (::type node)
     :value    (update node ::value f)
     :leaf     (update-leaf node f)
     :branch   (update-branch node f opts)
     :sequence (update-sequence node f)))
  ([node f]
   (update-node node f {})))

;;
;; Env Utils
;;

(defn with-error-handler
  [env handler]
  (update-vals env #(update-node % handler {:apply-to-condition? true})))

(defmacro try-env
  [env & body]
  `(try ~env
        ~@body))

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
