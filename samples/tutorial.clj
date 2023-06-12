(ns nodely.samples.tutorial
  (:require [nodely.api.v0 :as nodely :refer [>value >leaf >if >cond >sequence >and >or]]
            [clojure.core.async :as async]))

;; A value node is just a wrapper for any immediately available value
(def a-value (>value 1))
;; => #:nodely.data{:type :value :value 1}

;; A leaf node is a wrapper for a function with strong dependencies
(def a-leaf (>leaf (+ ?a ?b)))
;; => #:nodely.data{:type :leaf
;;                  :inputs #{:b :a}
;;                  :fn #function[nodely.samples.tutorial/fn--26449]}

;; We can try out individual nodes by filling in the dependencies with
;;
(nodely/eval-node-with-values a-leaf {:a 1 :b 2})
;; => 3

;; A branch node is able to express conditional dependencies
;; We can create a branch with >if, the first argument is a condition node,
;; the second argument is the truthy node and the third argument is the falsey node
;; At evaluation, if condition node is truthy, truthy node will be evaluated
;; otherwise, falsey node is evaluated
(def a-branch (>if (>leaf (even? ?a))
                   a-value
                   a-leaf))
;; => #:nodely.data{:type :branch,
;;                  :condition #:nodely.data{:type :leaf
;;                                           :inputs #{:a}
;;                                           :fn #function[nodely.samples.tutorial/fn--28863]}
;;                  :truthy #:nodely.data{:type :value :value 1}
;;                  :falsey #:nodely.data{:type :leaf
;;                                        :inputs #{:b :a}
;;                                        :fn #function[nodely.samples.tutorial/fn--28857]}}

(nodely/eval-node-with-values a-branch {:a 1 :b 2})
;; => 3
(nodely/eval-node-with-values a-branch {:a 2 :b 2})
;; => 1

;; Value nodes are automatically inferred
(def a-branch-2 (>if (>leaf (even? ?a))
                     1
                     a-leaf))
(:nodely.data/truthy a-branch-2)
;; => #:nodely.data{:type :value :value 1}

;; We can also define a branch with >cond
;; Similar to clojure.core/cond, if no clauses match, >cond evaluates to nil
(def another-branch (>cond (>leaf (even? ?a)) 1
                           :else              (>leaf (+ ?a ?b))))

(nodely/eval-node-with-values another-branch {:a 1 :b 2})
;; => 3

;; >and and >or are additional helpers for writing branches
;; They behave similar to their clojure counterparts
(def or-branch (>or false (>leaf ?a) 1))
(nodely/eval-node-with-values or-branch {:a false})
;; => 1
(nodely/eval-node-with-values or-branch {:a 2})
;; => 2
(def and-branch (>and true 1 (>leaf ?a)))
(nodely/eval-node-with-values and-branch {:a 2})
;; => 2
(nodely/eval-node-with-values and-branch {:a false})
;; => false

;; A sequence node is used to express a function mapping over a sequential value
;; The first argument is a dependency to a sequential value
;; The second argument is a function to be applied to each element of the sequence
(def a-sequence (>sequence inc ?a))

(nodely/eval-node-with-values a-sequence {:a [1 2 3]})
;; => [2 3 4]

;; An environment is a map of nodes

(def simple-env
  {:a (>value 1)
   :b (>value 2)
   :c (>leaf (+ ?a ?b))})

;; Evaluating a specific key of an env
(nodely/eval-key simple-env :c)
;; => 3

;; Evaluating a node in the context of an env
(nodely/eval-node simple-env (>leaf (* 3 ?b)))
;; => 6

;; We can eval a key on an env and return the env itself with the nodes that were evaluated changed into value nodes
(nodely/eval simple-env :c)
;; => {:a #:nodely.data{:type :value, :value 1},
;;     :b #:nodely.data{:type :value, :value 2},
;;     :c #:nodely.data{:type :value, :value 3}}

;; We can get extract a value from an env
(nodely/get-value (nodely/eval simple-env :c) :c)
;; => 3

;; (nodely/get-value simple-env :c)
;; => blows up because :c node is not a value

;; Running with different engines is possible
;; eval, eval-node and eval-key accept an option map with an engine
;; see the README for a more detailed description of each engine
(nodely/eval-key simple-env :c {::nodely/engine :sync.lazy})
;; => 3

;; It is not a rule, but typically the available engines
;; will only evaluate a node if it is actually a needed dependency
;; we can see that in practice by writing an env with a branch node

(def env-with-branch
  {:x (>leaf (+ 1 1))
   :y (>leaf (do (Thread/sleep 10000) 1))
   :z (>if (>leaf (even? ?x))
           (>leaf ?x)
           (>leaf ?y))})

(nodely/eval env-with-branch :z {::nodely/engine :sync.lazy})
;; => {:x #:nodely.data{:type :value, :value 2},
;;     :y #:nodely.data{:type :leaf,
;;                      :inputs #{},
;;                      :fn #function[nodely.samples.tutorial/fn--28898]},
;;     :z #:nodely.data{:type :value, :value 2}}

;; y represents an expensive call that we want to avoid when possible
;; In this case, y is not needed because (even? ?x) evaluates to true
;; So the dependency on why is still a leaf (unevaluated) and not a value

;;
;; Async Helpers
;; 

;; Some specific helpers are built to support engines powered by the core.async library
;; Async operations may return channels instead of straight values
;; >channel-leaf can be used to build leaf nodes from an expression that returns a channel
(def async-node (nodely/>channel-leaf (async/go (+ ?a ?b))))

;; All previous operations should be compatible
(nodely/eval-node-with-values async-node {:a 1 :b 2})
;; => 3

;; When using >channel-leaf, all other engines behave as if it was a normal leaf
;; However, when using core.async engines, they can take advantage of this for the purposes of
;; enabling non-blocking IO

;; eval-key-channel and eval-node-channel are counterparts or
;; eval-key and eval-node, respectively. They return a channel that will produce the answer instead
;; of the raw value

(def async-env
  {:a (>value 2)
   :b (>leaf (* 3 ?a))
   :c async-node})

(async/<!! (nodely/eval-key-channel async-env :c))
;; => 8
