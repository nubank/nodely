# Migrating an engine from a data-map to the `Engine` protocol

This guide is for continuing an in-progress refactor: converting each entry in
`nodely.api.v0/engine-data` from a plain data map into a type that implements
`nodely.engine.protocols/Engine`.

Three engines are already migrated — read them as worked examples before you
start:

- `:sync.lazy` → `nodely.engine.lazy/LazyEngine` — the **simple** case
  (no optional dependency).
- `:core-async.lazy-scheduling` →
  `nodely.engine.core-async.lazy-scheduling-engine/CoreAsyncLazySchedulingEngine`
  — the **optional-dependency, channel-SUPPORTING** case (the full pattern).
- `:core-async.iterative-scheduling` →
  `nodely.engine.core-async.iterative-scheduling-engine/CoreAsyncIterativeSchedulingEngine`
  — the **optional-dependency, channel-LESS** case (same facade pattern, but
  `-eval-key-channel` throws `UnsupportedOperationException`; see PITFALL 3b).

Do **one** engine at a time. Run the tests after each. Do not try to do several
at once.

---

## The protocol

`nodely.engine.protocols/Engine` has six methods. Every migrated engine must
implement all six:

```clojure
(-eval                        [engine env k opts]) ; -> resolved env
(-eval-key                    [engine env k opts]) ; -> value of k
(-eval-key-channel            [engine env k opts]) ; -> channel yielding value of k
(-eval-key-channel-supported? [engine])            ; -> true/false
(-enable-deref                [engine])            ; -> a delay (see below)
(-prepare-opts                [engine opts])       ; -> opts to pass to -eval*
```

The dispatch in `v0.clj` already routes any entry that has
`::protocol-engine? true`. You do not need to change `protocols.clj`.

---

## Before you write code: classify the engine

Look at the engine's existing entry in `engine-data`. **Does it have an
`::enable-deref` key?**

- **NO `::enable-deref`** (only `:sync.lazy` today) → the engine has no optional
  dependency. Put the `deftype` directly in the engine's implementation
  namespace, and make `-enable-deref` return `(delay nil)`. Copy `LazyEngine`.

- **HAS `::enable-deref`** (every other engine) → the engine depends on an
  **optional (`:scope "provided"`) library** (core.async, manifold, promesa,
  virtual-futures). This is the case with the pitfalls. Use the facade-namespace
  pattern below. Copy `CoreAsyncLazySchedulingEngine`.

---

## The optional-dependency pattern (facade namespace)

### PITFALL 1 — the deftype must load WITHOUT the optional dependency

`api/v0.clj` is always loaded, even when the optional library is absent. If the
deftype lived in a namespace that `(:require [clojure.core.async ...])` (or
manifold/promesa), then `v0.clj` referencing it would fail to load the moment
the library is missing. That defeats nodely's "works without the optional deps"
contract.

**DO:** create a new, small **facade namespace** whose only `:require` is
`nodely.engine.protocols`. It must NOT require the optional library, and must
NOT require any namespace that transitively requires it. Load the real
implementation **lazily**, inside the method bodies. See
`lazy_scheduling_engine.clj` for the exact shape:

```clojure
(ns nodely.engine.<family>.<engine>-engine
  (:require [nodely.engine.protocols :as engine.protocols]))

(def ^:private impl-ns 'nodely.engine.<family>.<engine>) ; the real impl

(def enable-deref
  (delay
   (try (require impl-ns) nil
        (catch Exception e
          {:msg "Could not locate <lib> on classpath." :cause e}))))

(defn- impl [fn-name]
  (requiring-resolve (symbol (name impl-ns) (name fn-name))))

(deftype <Engine> []
  engine.protocols/Engine
  (-eval             [_ env k opts] ((impl 'eval) env k opts))
  (-eval-key         [_ env k opts] ((impl 'eval-key) env k opts))
  ;; -eval-key-channel: pick ONE of the two shapes below -- see PITFALL 3b.
  ;;   channel-SUPPORTING engine (impl HAS an eval-key-channel fn):
  (-eval-key-channel [_ env k opts] ((impl 'eval-key-channel) env k opts))
  ;;   channel-LESS engine (impl has NO eval-key-channel fn):
  ;;   (-eval-key-channel [_ _ _ _]
  ;;     (throw (UnsupportedOperationException. "Engine :the-engine does not support eval-key-channel.")))
  (-eval-key-channel-supported? [_] <true-or-false>)
  (-enable-deref     [_] enable-deref)
  (-prepare-opts     [_ opts] <see PITFALL 3>))
```

### PITFALL 2 — `-enable-deref` is the real gate; the order is construct → check → eval

`-enable-deref` answers "can this engine run on this classpath?". Because you
must construct the instance before you can call a method on it, and because the
facade constructs without the optional library, the correct order is:

1. **construct** the engine instance,
2. **deref `-enable-deref`**; if it returns a failure map, throw,
3. only then call `-eval` / `-eval-key` / `-eval-key-channel`.

The dispatch already does this through the `protocol-engine` helper in `v0.clj`.
You do not need to re-derive it — just make sure your `-enable-deref` returns a
`delay` that attempts `(require impl-ns)` and reports the failure.

Do NOT try to check availability *before* constructing, and do NOT put the
availability check anywhere that requires loading the optional library first.

### PITFALL 3 — `-prepare-opts` must reproduce the old `::opts-fn`

Look at the engine's current `::opts-fn` and make `-prepare-opts` do the same
thing. Do **not** blindly copy `LazyEngine`'s `nil`.

- `::opts-fn identity`            → `(-prepare-opts [_ opts] opts)`
- `::opts-fn (constantly nil)`    → `(-prepare-opts [_ _opts] nil)`
- `::opts-fn #(assoc % ::applicative/context ...)` → reproduce that `assoc`
  inside `-prepare-opts` (and note the `(resolve '...context)` runs lazily,
  which is fine because `-enable-deref` has already confirmed the library is
  present by the time `-prepare-opts` is called).

### PITFALL 3b — channel-LESS engines must THROW from `-eval-key-channel`

**Classify first:** does the engine's data-map entry have `::eval-key-channel
true`? Equivalently, does its impl namespace define an `eval-key-channel` fn?

- **YES (channel-supporting)** → `-eval-key-channel-supported?` returns `true`
  and `-eval-key-channel` delegates: `((impl 'eval-key-channel) env k opts)`.
  Keep `::eval-key-channel true` in the v0 entry.

- **NO (channel-less)** — e.g. `:core-async.iterative-scheduling`,
  `:async.manifold`, `:applicative.promesa` → `-eval-key-channel-supported?`
  returns `false` and `-eval-key-channel` must
  `(throw (UnsupportedOperationException. "Engine :the-engine does not support eval-key-channel."))`.
  **Do NOT** copy the delegating body: `(impl 'eval-key-channel)` resolves to
  `nil` for a channel-less impl, so calling it throws a bare, uninformative
  `NullPointerException` ("cannot invoke nil"). That NPE is what the old
  data-map dispatch did by accident; the migration is the chance to replace it
  with an intentional `UnsupportedOperationException`. **Omit** `::eval-key-channel`
  from the v0 entry entirely (do not set it `false`; the key's presence is what
  the tests read via `channel-interface`).

  Worked example: `iterative_scheduling_engine.clj` is the channel-less
  core.async engine — read it alongside `lazy_scheduling_engine.clj` (the
  channel-supporting one) to see the two shapes side by side.

---

## Editing `api/v0.clj`

1. `:require` the new facade namespace (keep the require list alphabetical, or
   `lein clean-ns` will complain).
2. Replace the engine's data-map entry with:
   ```clojure
   :the-engine {::protocol-engine?     true
                ::instance-constructor <facade-ns>/-><Engine>
                ::eval-key-channel     <true-or-omit>}
   ```
3. Do **not** delete the shared failure delays.

### PITFALL 4 — do not delete shared `*-failure` delays

`core-async-failure` is used by `:applicative.core-async` and the
`>channel-leaf` macro (it was also used by `:core-async.iterative-scheduling`
until that engine was migrated). The other `*-failure`
delays are likewise shared. Migrating one engine does not free its delay. Each
migrated engine gets its **own** `enable-deref` in its facade namespace; leave
the `v0.clj` delays alone until every engine that uses one is migrated.

### PITFALL 5 — keep ONE `let` level in `eval` / `eval-key` / `eval-key-channel`

The dispatch functions already have the right shape. Do not add a nested `let`.
The instance is bound once, guarded by `when`:

```clojure
(let [engine-data      (engine-data engine-name)
      protocol-engine? (::protocol-engine? engine-data)
      engine           (when protocol-engine? (protocol-engine engine-name engine-data))]
  (if protocol-engine?
    (engine.protocols/eval engine env k (engine.protocols/-prepare-opts engine opts))
    (let [efn (engine-fn engine-name 'eval)]
      (if-let [opts ((::opts-fn engine-data) opts)]
        (efn env k opts)
        (efn env k)))))
```

If you already migrated an engine, these three functions need NO further change
— they are generic.

---

## Editing the tests

### PITFALL 6 — the graceful-degradation test must target the engine's OWN delay

`test/nodely/api_test.clj` simulates "library missing" by bombing `require` and
resetting a delay. A migrated engine no longer consults the `v0.clj`
`*-failure` delay — it consults its **own** `enable-deref` in its facade
namespace. So for the engine you migrate:

- point the `testing-require-delay` block's bombed namespace at the engine's
  real implementation namespace (the one its `enable-deref` requires), and
- point the reset delay at `<facade-ns>/enable-deref`.

The reset helpers (`ensure-unrealized-delay` and the end-of-block reload) already
reload the delay's **own** namespace via `(symbol (namespace sym))`, so passing a
facade-namespace delay works without further change. Keep the assertion that the
engine throws its "Could not locate ..." message.

---

## PITFALL 7 — the applicative-family engines are NOT simple; do them last

`:applicative.promesa`, `:applicative.core-async`, and
`:applicative.virtual-future` all share the implementation namespace
`nodely.engine.applicative`, which **itself requires `clojure.core.async`** and
injects a per-engine "context" resolved from an optional namespace. That means
the facade-must-not-transitively-require-the-optional-dep rule is harder to
satisfy, and `-prepare-opts` must reproduce the context injection. Do the
standalone engines first (`:core-async.iterative-scheduling`, `:async.manifold`,
`:async.virtual-futures`). Ask a human before attempting the applicative family.

---

## Verify (PITFALL 8 — do all of these, every time)

Run these after each engine. Do not skip.

1. **Parse** every file you touched:
   `bb -e '(require (quote [rewrite-clj.zip :as z])) (z/of-file "PATH")'`
2. **Format / namespaces:** `lein format` and `lein clean-ns` (both dry) — must
   report nothing to change.
3. **Tests:** `lein test`. Expect `0 failures, 0 errors`.
   - The one test in `nodely.engine.manifold-test` that compares timing is
     **flaky** (it allows only an 8ms tolerance on a ~2-second measurement). If
     it — and only it — fails on timing, just run `lein test` again. Any other
     failure is a real regression.
4. Re-run the touched namespaces a couple of times
   (`lein test nodely.api-test nodely.profile-test`) to confirm the test
   reset logic is stable.

Do not commit. Leave commits to a human reviewer.
