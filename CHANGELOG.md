# Changelog

## 1.13.0 / 2023-09-12
- Add checked-env function for cycle detection in env
- Added applicative synchronous engine (:async.applicative)
- Applicative engine support for core-async
- Applicative engine support for sync execution
- Add schema validation syntax
- Add support for schema validation on applicative engine

## 1.12.0 / 2023-02-08
- Add an applicative/monad engine using funcool/promesa in the backend, but theoretically extendable to anything implementing applicative and monad protocols. This is experimental and comes with no guarantees. Some minor tweaks might be needed to support cljs.

## 1.11.0 / 2022-11-24
- Add data constructors to api
- solve #89 

The raw constructors are useful to build nodes dynamically

## 1.10.0 / 2022-10-17
- Add nodely tutorial
- add eval to api.v0
- fix >or and >and

## 1.9.0
- Add >if helper with similar behavior to clojure `if`

## 1.8.2
- Lazy Scheduler Core.Async Engine throws exception when looking for a key that is missing
- Fixes #67 

## 1.8.1
- Lazy Scheduling Core.Async Engine Optimization
- Remove some unnecessary go blocks by passing out-ch explicitely to eval-async

## 1.8.0
- Async Leaf Interface: AsyncThunk is a record that receives a function that returns a channel. With this, AsyncThunk implements `evaluation-channel` and `invoke`. The former is needed for non-blocking async behaviour. Invoke is needed to keep compatibility with sync.lazy engine and other engines relying on sync call behaviour. Helpers for creating leaf returning a channel is provided.
- Core Async Exception Handling: We're using an exception channel for fast tracking exceptions and end execution early without bubbling up exception through channels. This required some machinery to make the job easier.

## 1.7.1
- Fix flaky test (collision on keywords on sequence-gen having same value)

## 1.7.0
- Add api for evaluation returning a channel

## 1.6.0
- Use the `evaluation-channel` protocol function to get values out of
the fn position on nodes.
- Should allow e.g. async http request thunks to work right without
any more protocol fns.
- You have to close channels in `async-pipeline`!!! Otherwise it
blocks indefinitely.

## 1.5.4
- On lazy scheduling core async engine, extended IFn protocol so that it implements sync/async protocol
- Make lazy scheduling core async engine use async protocol by default

## 1.5.3
- Fix error where engine swallows nil values when executing a sequence

## 1.5.2
- [Fix] Add back iterative scheduling

## 1.5.1
- Change the api so that there is only one eval-key with options to customize the engine.
- Add api.v0 namespace
- Add core-async engine with lazy scheduling support

## 1.5.0
- Describe your change here.

## 1.4.0
- Add >value helper
- remove prismatic/schema-generators from core deps

## 1.3.0
- All evals return a channel
- Pipe channel results to the promise channel in the graph
- Eval leaf and eval node run trivial go blocks to get their returns
  in channels.
- Eval seq sets up parallel pipeline to process the sequence in
  parallel.
- `nodely.engine.core-async/*eval-sequence-parallelism*` affords
  client control of sequence parallelism, defaults to 4 pipelines.

## 1.2.0
- Added commited-dependencies function, a helper for efficiently processing nodely graphs async
- Leaf inputs are now called `::inputs` and accept a set
- Sequence inputs are now called `::input` and accept a keyword

Handles https://github.com/nubank/nodely/issues/54

## 1.1.0
- Add core async engine implementation. This engine unravels the branches sync first before solving the dag with core async. Sequences are still using manifold futures.

## 1.0.0
- Change branch data structure to allow node on condition
- Simplify syntax macros
- Change branch data structure attribute from left -> truthy and right -> falsey

This change allows the user to express conditions as nodes, and also include branches and branch-like structures on this condition. Example:
```clojure
(>cond
  (>and (>leaf (check-1 ?a))
            (>leaf (check-2 ?b)))

  (>leaf (do-something ?a ?b))

  (>leaf (check-3 ?c)
  (>leaf (do-something-else ?d))
```

The trade off is some added verbosity for extra simplicity and consistency.

Addresses https://github.com/nubank/nodely/issues/4

## 0.9.0
- Fixes sequence schema

## 0.8.0
- Enable support for sequences

## 0.7.2
- Use recursive schema declaration

## 0.7.1
- Fix env macro to enable declaration of partial envs

## 0.7.0
- Support direct node definition on cond and env by escaping inner node with `>node` symbol.

## 0.6.0
- Refactor api and add helpers for values

## 0.5.0
- Support nested conds.

## 0.4.2
- Remove unnecessary var quote.

## 0.4.1
- Switch async implementation to use deferred/future from manifold instead of future. Deferred/future uses a default executor that may or may not start a new thread depending on thread pool utilization. It is possible to customize executor in the future if necessary to provide additional thread pool control.

## 0.4.0
- Add async execution engine
- Reorg engine ns into core, sync and lazy ns
- Change api to reflect async vs lazy eval

## 0.3.1
- Fix cond expansion bug

## 0.3.0
- Add public api namespace

## 0.2.9
- fix wrong data schema

## 0.2.8
- Fix bug that was generating leaf with a 0-arg function for leaf with no input

## 0.2.7
- Fixing release

## 0.2.6
- fix eval-with-map test

## 0.2.5
- fix broken eval and add tests

## 0.2.4
- cond and env was incorrectly expanding expr with no dependencies as values, but they should be leaves to defer evaluation

## 0.2.3
- add engine and syntax namespace

## 0.2.2
- Add data namespace and schemas

## 0.2.1
- Fix cond-qm

## 0.2.0
- Add cond with question mark syntax

## 0.1.1
- Empty change to fix pipeline

## 0.1.0
- First version

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).


## [Unreleased]
### Added
- _CHANGELOG.md_ created.
### Changed
- Something has been changed.
### Fixed
- Something has been fixed.
### Removed
- Something has been removed.


[Unreleased]: https://github.com/nubank/nodely/compare/0.0.0...HEAD
