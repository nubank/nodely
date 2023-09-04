# Nodely

[![Clojars Project](https://img.shields.io/clojars/v/dev.nu/nodely.svg)](https://clojars.org/dev.nu/nodely)

Declarative data dependency graph with conditionals. See the [tutorial](samples/tutorial.clj) for guided code examples.

## Status

Nodely is still in beta. We don't expect it to change it by a lot, but expect some rough edges.

## Introduction

Nodely is a library for declaring and executing data dependent computations with support for conditional dependencies. Declaration of the graph is completely decoupled from its execution, allowing for multiple execution stategies (engines) to be implementable. Many tools exist for computation of [DAGs](#directed-acyclic-graphs), but few of them support conditional dependencies.

### Main Features

* Declarative data dependency graph
* Conditional data dependency
* Async data dependency resolution
* Lazy data dependency resolution

### Rationale & Motivation

The development of the library was motivated by those main problems:
* How can we maximally decouple pure logic from effectful computation?
* Separate data dependency declaration from the data fetching execution model
* Support expression of conditional logic

There are many reasons why we would want to solve any of these problems. Here we mention just a few of them:

By decoupling the way we declare the work to be done and the way it is run gives the programmer the option to quickly switch execution engines with few code changes. Async errors, for instance are known to be hard to debug. Switching from async to lazy/sync engine is a way for the programmer to understand the source of an error and if they are caused by asyncrony or not.

By separating how we fetch the data from the logic that uses it enables unit testing your graph in a way that is decoupled from the means of fetching the data, which might involve a query to a database or a http request.

Finally, supporting the expression of conditional logic enables us to express that "Depending on the result of computation `a` that depends on `X`, I will do computation `b` that depends on `Y` or I will do computation `c` that depends on `Z`" without coupling the expression to the way X, Y and Z are acquired. It is a step towards further decoupling of the logic, its data dependencies and its actual execution.

### Directed Acyclic Graphs

Nodely does not work in Directed Acyclic Graphs (DAGs), but the
_environments_ that run in Nodely are a superset of DAGs. Thus, DAGs are
a useful basis for reasoning about Nodely _environments_.

Nodely _environments_ are similar to DAGs in the following ways:

- The edges between nodes are expressed in dependencies, and this
  expression creates a single unambiguous direction for the edge.
- The graph may not contain cycles: Nodely uses a dependency
  resolution process to evaluate nodes. If Nodely accepted and
  attempted to evaluate a cycle, it would loop indefinitely (see [Cycle Detection](#cycle-detection)).
- The combination of nodes and their dependencies instructs Nodely how
  to evaluate some target node.

Nodely _environments_ differ from DAGs primarily in embodying
branching logic. Some Nodes may be "branch" Nodes, and express an
uncertainty of the graph that may not be resolved until run time. By
expressing branches; Nodely allows for conditional dependencies that
defer evaluation until run time checks have determined it is necessary
to evaluate a dependency.

### Cycle Detection

Nodely provides a handy function that throws an error when a cycle has been detected in your _environment_. To use this function, simply execute the following code:

```clj
(comment
  (require '[nodely.api.v0 :as nodely])
  (nodely/checked-env
   {:a (nodely/>value 1)
    :b (nodely/>leaf (even? ?a))})
) ;; When no cycle is detected, returns env {:a #:nodely.data{:type :value, :value 1}...}
```

>Avoid using the `checked-env` function at runtime, as its calculations can be costly. It is recommended to perform this verification only during development, using the REPL.

It's important to be aware that this function may sometimes produce false positives when dealing with graphs that contain mutually exclusive conditions. Here's an example where the function might produce a false positive:

```clj
(comment
  (require '[nodely.api.v0 :as nodely])
  (nodely/checked-env
   {:f  (>if (>leaf ?c) :it-was-even! (>leaf ?e))
     :e (>if (>leaf ?c) (>leaf ?f) :it-was-odd!)
     :c (>leaf (even? (rand-int 2)))})
) ;; throws "Checked-env found cycles at compile time"
```

Keep in mind these limitations and double-check the results if your graph contains such scenarios.

## Definitions

### Nodes
A node can be a:
* Value
* Leaf
* Sequence
* Branch

#### Value

This is the simplest node, it represents an immediately available value. It is used internally for representing computations that were already carried through and it can also be used to represent given values or simple calculations that do not depend on other nodes. Examples:

```clojure
;; Simple value
(nodely/>value 1)
;; Simple calculation without dependencies on other nodes
(nodely/>value (* 2 3))
```

#### Leaf

Leaf nodes are the node equivalent of functions. A leaf may have no dependencies, representing some computation that is going to be evaluated at run time and it may have dependencies. All dependencies are strong dependencies (in opposition to conditional dependencies), meaning that they will definitely be necessary for the node to be evaluated. Examples:

```clojure
;; Leaf with no dependencies
(nodely/>leaf (expensive-call))
;; Leaf that depends on node `a` and `b`
(nodely/>leaf (+ ?a ?b))
```

#### Sequence

Sequences nodes are roughly equivalent to a `map` over a sequential value. They are used to express a computation that needs to be carried over for each element of the sequence input. The key advantage of having that kind of computation expressed as a node is that nodely can optimize execution by running the computations concurrently. The exact execution strategy may change depending on the engine being used.

```clojure
;; Env with sequence
(def my-env
  {:a (nodely/>value [1 2 3])
   :b (nodely/>sequence ?a (fn [each] (inc each)))})
(nodely/eval-key my-env :b)
;; => [2 3 4]
```

#### Branch

Branch nodes are how we express <italic>conditional data dependency</italic>. The reason for their existence is better explained with an example:

```clojure
(defn my-fn
  [x y]
  (if (even? x)
    x
    y))
```

`my-fn` is a function that requires two arguments (x and y). In practice though, only x is always needed. If `(even? x)` is `true`, we don't need `y` at all. You might want to say that "y requirement is conditionally dependent on x". Typical function declaration on Clojure does not have enough metadata to describe that conditional dependency. Here comes nodely:

```clojure
(def branch-node
  (nodely/>if (>leaf (even? ?x))
    (>leaf ?x)
    (>leaf ?y)))
```

This is syntactic sugar for declaring a `branch node`. By inspecting the var, we can see that it is just a map:

```
> branch-node
=> #:nodely.data{:type :branch,
                 :in [:x],
                 :condition #:nodely.data{:type :leaf,
                                          :in [:x],
                                          :fn #function[fn--15482]}
                 :truthy    #:nodely.data{:type :leaf,
                                          :in [:x],
                                          :fn #function[fn--15486]},
                 :falsey    #:nodely.data{:type :leaf,
                                          :in [:y],
                                          :fn #function[fn--15492]}}
```

The branch node has four fields: 

`type`: The type of the node, in this case, `:branch`

`in`: The input keys of the node. Only what is needed to execute the `condition` appears here.

`condition`: the node representing the condition of the branch; `(>leaf (even? x))`

`truthy`: the node to replace the node branch in case condition returns `truthy`

`falsey` the node to replace the node branch in case condition returns `falsey`

The leaf nodes on the `:truthy` and `:falsey` keys do not have any `:condition`, so the `:in` field on leaf nodes specify strong dependency (not conditional). As you can see, in the whole map, the dependency on `y` only appears on the `:falsey` node and nowhere else. Internal details aside, what this enables is a declarative way of expressing weak/conditional data dependencies clearly. 

### Environment

An environment is just a map of nodes. Consider this branch node:

```clojure
(def branch-node 
  (nodely/>if (>leaf (even? ?x))
    (>leaf ?x)
    (>leaf ?y)))
    
(defn my-env {:x (>leaf (request-for-x ...))
              :y (>leaf (request-for-y ...))
              :z branch-node})
```

Let's say that `x` and `y` are actually expensive http requests. What we want is to make sure we don't execute the request for `y` unless we actually need it. We can evaluate a node within an environment by running:

```clojure
(nodely/eval-key my-env :z  {::nodely/engine :sync.lazy})
```

This will evaluate the node by realizing all required dependencies, without executing the dependencies that are not needed. For instance, if x is even, y is not evaluated at all.

#### Testing

Using `eval-node-with-values` is the most straightforward way to evaluate a node by supplying actual values for the data dependencies. If we want to test `branch-node` without actually making the expensive calls, it is as simple as that:

```clojure
(nodely/eval-node-with-values branch-node {:x 2 :y 3})
;=> 2
```

### Core Async Support

Nodely implements partial support for non blocking operations with [core-async](https://github.com/clojure/core.async). The main helper for building async nodes is `>channel-leaf`. It is similar to `>leaf`, but expects the enclosured expression to return a core-async channel:

```clojure
(def env {:a (>value 1)
          :b (nodely/>channel-leaf
               (async/go (+ ?a 5)))})
(nodely/eval-key env :b {::nodely/engine :core-async.lazy-scheduling})
;=> 6
```
The example above is just to demonstrate the syntax. The main benefit of using `>channel-leaf` is for wrapping it around async IO operations. `>channel-leaf` is compatible with all other engines, but to take full advantage of non-blocking IO you should use one of the core-async engines, such as `:core-async.lazy-scheduling`.

## Evaluation

Defining graphs of data dependency is of marginal value unless we can
apply those definitions to arrive at answers. Consequently, Nodely
defines multiple engines which will evaluate graphs to arrive at a
value for a targeted node.

Engines supported in Nodely include:

### :sync.lazy

The Nodely `:sync.lazy` engine executes in **one thread** of
execution, visiting nodes in depth-first traversal from the targeted
node to terminal nodes. Only those nodes necessary to evaluate the
specified target will be evaluated. Blocks execution of the calling
thread until a value is produced.

This engine should be the least complicated to reason about, and
useful for debugging. It will schedule nodes to evaluate strictly head
and tail, and may introduce accidental latencies consequently.

### :async.manifold

The Nodely `:async.manifold` engine executes in the context of
[Manifold Deferred
Futures](https://github.com/clj-commons/manifold/blob/master/src/manifold/deferred.clj#L660). First,
all branches in the environment are synchronously resolved to
determine one DAG. Then, all nodes are immediately scheduled to eval
in parallel, with dependencies managed by inter-thread blocking.

This engine may afford the greatest concurrency of evaluating nodes
and will likely produce a result in the smallest amount of wall clock
time. However, it is likely to create many blocking Threads in the
process, and so may have poor consequences when, e.g. serving many
hundreds or thousands of requests per minute.

### :core-async.lazy-scheduling

The Nodely `:core-async/lazy-scheduling` engine executes in Clojure's
`core.async` library. Core async jobs are spawned to evaluate
indvidual nodes lazily, as those nodes are determined to be required
to satisfy the target node. When a branch condition is not yet
resolved, one job will be spawned to evaluate the branch condition,
and at each step of branching, new jobs will be scheduled to resolve
all nested branch conditions.

This engine will minimize the number of blocking Threads, and afford
good concurrency for graphs that do not imply sequencing of
evaluation. However, in degenerate cases it may result in scheduling
work head and tail, while incurring additional costs of running in
core.async.

When functions are specified in nodes with the
`:core-async/lazy-scheduling` engine, the functions must not block, or
they will block in the `core.async` Dispatch Thread Pool and present a
major performance penalty.

## Risks

Nodely presently is built to support acyclic graphs; this means that
the graph does not have a loop. Put another way, the following env
would cause problems:

```clojure
{:a (nodely/>leaf (+ ?b 1)) :b (nodely/>leaf (- ?a 1))}
```

`:a` depends on `:b`, and `:b` depends on `:a`. Nodely neither
resolves this kind of loop in any way (as in the above, how could
it?), nor does it detect these loops proactively and abort evaluation
when provided them. Loops in environments will fail if they're on the
path of evaluation, and should be avoided.

While the presence of loops in evaluation is not defined, the
consequences in most nodely engines are that evaluation will hang and
no result will be produced. Sometimes this may be accompanied by a
stack overflow error from mutual recursion, other times this will
result in the process hanging without even consuming much CPU as two
core.async workers deadlock, waiting on each other to complete.

If you see evaluation hanging, start investigating for loops in your
environment.

## Related Work

From our knowledge there are some libraries with similar functionality, but none of them do exactly what nodely proposes

### [Plumbing Graph](https://github.com/plumatic/plumbing#graph-the-functional-swiss-army-knife)

Supports declaring data dependency graphs with lazy and async execution. Does not support expressing conditional data dependency. Does not support expression of data dependency over sequential data.

### [Pathom3](https://github.com/wilkerlucio/pathom3)

In Pathom3 we can define resolvers that represent knowledge of how to fetch/process some data. By defining resolvers we can express relationships between data attributes. Pathom then takes care of choosing the resolution paths.

Even though there is some similarity and intersection between Pathom3 and Nodely, as far as we can understand, we think that Pathom3 is much larger, complex, opinionated and tightly coupled with itself. Pathom3 relies a lot on dynamic behavior and mutability, something that Nodely tries to avoid.

Contrary to Pathom3, Nodely does not aim to be a full fledged out of the box framework. It is supposed to solve a particular problem in a functional declarative style. Nodely could be used as backend functionality for more complete data resolution engines or systems.

### [Manifold](https://github.com/clj-commons/manifold)

Manifold offers the `let-flow` macro, which enables data dependency declaration that are automatically resolved and can be executed asyncronously.

However, manifold does not decouple the data dependency declaration from the execution model and also does not support conditional data dependencies.

### [Domino](https://github.com/domino-clj/domino)

### [Twitter Nodes](https://github.com/twitter/nodes)

### Lazy Map
