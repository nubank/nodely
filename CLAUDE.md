# Migrating an engine from a data-map to the `Engine` protocol

This guide is for continuing an in-progress refactor: converting each entry in
`nodely.api.v0/engine-data` from a plain data map into a type that implements
`nodely.engine.protocols/Engine`.

These engines are already migrated — read them as worked examples before you
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
- The **applicative family** (`:applicative.promesa`, `:applicative.core-async`,
  `:applicative.virtual-future`) → a single
  `nodely.engine.applicative.engine/ApplicativeEngine` — the **one-type,
  many-contexts** case: all three are ONE deftype parameterized by their
  applicative context symbol + gate, and all are channel-SUPPORTING. See
  PITFALL 7 for the design and the `redeem` combinator that makes their
  `eval-key-channel` deliver exceptions.

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
  `:async.manifold` → `-eval-key-channel-supported?`
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

## PITFALL 7 — the applicative family: ONE type, many contexts (MIGRATED)

`:applicative.promesa`, `:applicative.core-async`, and
`:applicative.virtual-future` all share the implementation namespace
`nodely.engine.applicative`, which **itself requires `clojure.core.async`** and
injects a per-engine "context" (the applicative instance — the monad) resolved
from an optional namespace. Because the shared impl is already generic over that
context, the three engines are **not** three deftypes — they are ONE:
`nodely.engine.applicative.engine/ApplicativeEngine`, a two-field deftype
`[context-sym enable-deref-delay]` living in a namespace that requires only
`nodely.engine.protocols`. Three zero-arg factory fns
(`->promesa-applicative-engine`, `->core-async-applicative-engine`,
`->virtual-future-applicative-engine`) construct the one type with the right
context symbol + gate. Read `engine.clj` as the worked example.

Key points specific to this family:

- **All three are channel-SUPPORTING** (this retires the old claim that
  `:applicative.promesa` was channel-less). `-eval-key-channel-supported?` is a
  constant `true`; all three v0 entries carry `::eval-key-channel true`.
- **`-prepare-opts`** injects the context as the fully-qualified literal keyword
  `:nodely.engine.applicative/context` (NOT `::context` — this ns has no alias to
  the shared impl), resolving the context lazily via
  `(var-get (requiring-resolve context-sym))` after the gate has confirmed the
  optional lib.
- **`redeem` — how `eval-key-channel` delivers exceptions.** The shared
  `nodely.engine.applicative/eval-key-channel` used to do
  `(app/fmap (partial async/put! chan) contextual-v)`, which **deadlocks** on a
  failed node: every applicative verb except `extract` conveys the throwable AS
  its monadic value WITHOUT invoking the fn, so on error `put!` never runs and a
  reader of the channel blocks forever. The fix is `redeem` (named per
  cats-effect): the non-blocking, *ungated* sibling of `extract` — it invokes the
  fn on the resolved monadic value whether that value is a success OR the
  throwable. `eval-key-channel` is now
  `(app/redeem (partial async/put! chan) contextual-v)`. `-redeem` is a new
  protocol (`applicative/protocols.clj`) with a per-context impl, each using its
  native non-blocking completion (no new OS threads):
  - core-async: `(go-future (f (<! mv)))` — raw parking `<!` (not `<?`), so the
    throwable arrives as a value;
  - promesa: `(pp/-handle mv (fn [v e] (f …)))`;
  - virtual-future: `(vfuture (f (try (deref-unwrapped mv) (catch Throwable t t))))`
    — its native `vfuture`.
  The delivered exception is the ORIGINAL throwable (ex-data intact), matching how
  the sync `eval-key` (via `extract`) surfaces it and how the
  `:core-async.lazy-scheduling` engine delivers channel errors. Regression test:
  `applicative-eval-key-channel-delivers-exceptions` in `api_test.clj`.
- **`promesa-failure` was removed from `v0.clj`** — once all applicative engines
  became protocol engines it had zero remaining consumers. `core-async-failure`
  (used by `>channel-leaf`), `virtual-future-failure` (`:async.virtual-futures`),
  and `manifold-failure` (`:async.manifold`) stay.

Remaining standalone engines still to migrate: `:async.manifold` (channel-less,
PITFALL 3b) and `:async.virtual-futures` (channel-supporting).

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

---

# Design: engine eval hooks and `profile-eval` (resolved notes, 2026-11-05)

Purpose: extend the `Engine` protocol surface so an engine can run hooks around
each execution of **user-defined code**, to profile the time it takes to
resolve an environment. These notes resolve the open questions in the
`(comment ...)` block of `src/nodely/engine/protocols.clj`. Nothing here is
implemented yet.

Status markers: **[decided]** = agreed with Alex. **[proposed]** = proposed in
design discussion, not yet explicitly confirmed. Confirm these before building
on them. **[finding]** = derived by formal (runes) analysis of the code.

## Scope and access

- **[decided]** Callers do not install hooks. The public API exposes a single
  profiling operation (working name `profile-eval`) built on the hook protocol.
  General tool, narrow access.
- **[decided]** Hooks are **fields on the engine types**. The zero-arg
  constructors in `api/v0/engine-data` may change, because they are not a
  promise to clients.
- **[finding]** Each engine has **one pre-hook slot and one post-hook slot**.
  The draft separate methods `-pre-eval-hook` / `-post-eval-hook` stay as
  drafted. Installing again overwrites. Multiple hooks would only uniquely
  offer composition by installers who don't know about each other, which is
  out of scope under narrow access. Anything else (ordering, per-hook tokens,
  isolating a failing hook) is a composite fn built outside the engines. Pre/post
  pairing is correct by construction as long as both come from one installer;
  `profile-eval` is the only installer.

## What a hook observes (the "subject")

- **[decided]** A subject is at least as narrow as a branch's `condition`. If a
  condition is slow, the profile must show that it is the condition.
- **[decided]** A subject is identified by **event kind + path**.
  **[finding]** The path alone is not unique: sequence start and finish share
  their node's path. Events:
  - `:leaf` — a leaf's fn, wherever it occurs (env value, branch condition or
    arm, nested branches, a sequence's process-node). Example paths: `[:k]`,
    `[:k :condition]`, `[:k :condition :falsey]`, `[:k :process]`.
  - `:element` — one call of a sequence's fn on one element; path `[:k i]`.
    Fires per element even when the process-node is a plain `:value` fn (which
    `nodely.profile` cannot time today).
  - `:seq-start` / `:seq-finish` — the span of the whole sequence. This
    includes parallel overlap and pipeline waiting, so it is **not** user-code
    time; name and document it as a span.
  - `:value` nodes emit no events, since they run no user code.
- **[decided]** `profile-eval` annotates each node's path into **node
  metadata** before evaluation. Engines do not need to pass envs through
  unmodified. Engines don't compute paths; they read them off the node.
  `vary-meta` in `core/expand-branches` and arm substitution in `unbranch`
  carry the per-node metadata along.

## What is measured

- **[decided]** Only the time to **execute user code** (the `fn` or other
  evaluable thing). No dependency waiting, no scheduling or queueing. The pre-hook
  fires **inside** the execution context (inside the `meander` go block, inside
  `async/thread` for `blocking`, inside the future or applicative context), not
  where work is scheduled.
- **[decided]** When user code returns a monadic completion (a core.async
  channel from `>channel-leaf`/`AsyncThunk`, or an applicative leaf returning a
  promise or channel), the time at which the call returns is wrong, because the
  work isn't done yet. The post-hook fires on **delivery**, via a wrapped
  channel or completion. The callback-dispatch lag this adds is an accepted
  error. **[finding]** Every other kind of invocation is a direct call, and
  post fires when the call returns. "When the engine's own read observes the
  value" is never correct, because it always includes waiting.

## Hook placement: closure scope, not metadata

- **[decided]** Hooks travel through **closure scope at the call site**.
  Metadata is used only where a closure cannot work.
  **[finding]** A closure works at all 8 places where user code is invoked:
  `core/eval-leaf`, `core/sequence->value`, manifold and virtual-workers
  `eval-sequence`, iterative `eval-sequence`, lazy-scheduling leaf and
  sequence element, applicative `eval-leaf` and sequence element. At each of
  them the node, the hooks and the element index are in scope, and the wrapper
  can preserve what the callee dispatches on:
  - an `AsyncThunk` stays an `AsyncThunk` around a wrapped `channel-fn`
    (lazy-scheduling's `FnToChannel` and applicative core-async's
    `instance? AsyncThunk` dispatch on the type);
  - the wrapper copies `f`'s metadata, because applicative `eval-leaf` /
    `-apply-fn` read `::data/tags` from it. That is nodely's existing tags
    mechanism, not hook transport.
  Do **not** replace `::data/fn` in the env with a timing closure the way
  `nodely.profile` does. That changes how an engine executes it, which is why
  `ProfiledAsyncThunk` had to exist.
- **[proposed]** The sync paths (`:sync.lazy`, and the `core/unbranch-all`
  used by manifold and virtual-workers) run user code **inside
  `nodely.engine.core`**. Branch conditions, the conditions' env dependencies
  (via `resolve-inputs` → `core/resolve`), and in `:sync.lazy` the branch arms.
  **[finding]** An engine-local `unbranch` that delegates to `unbranch-all`
  cannot see inside the delegation. Reimplementing the branch step covers
  conditions but not their dependencies. So the hooks enter `core` as an
  **optional extra arity** threaded through the resolve fns. Existing arities
  are unchanged. Prefer this over a dynamic var: it is explicit, and nobody has
  verified that manifold's `deferred/future` conveys bindings.
- **[proposed]** Each engine passes its hook fields to its implementation fns
  under a **private opts key**, set inside its own `-eval*` methods (not by the
  caller, not by `-prepare-opts`). This is like how the applicative family
  passes its context.

## Hook contract

- **[proposed]** `pre: (fn [subject] -> token)` and
  `post: (fn [subject token result] -> ignored)`, with
  `subject = {:event ... :path [...] :node node}` (+ the element index for
  `:element`). The engine routes each pre's token to **that same invocation's**
  post (on its own stack or in a closure, never shared state).
  **[finding]** Recording the start time in shared state keyed by path
  mismatches pre and post under concurrency in 4 of 6 interleavings. Returning a
  token from pre is correct in all 6.
- **[decided]** Post fires on **every** way user code can end: return, throw,
  delivering a value, delivering a throwable, closing without a value. A
  throwing user-code block still triggers post (`result` carries the throwable,
  or an explicit outcome field).
- **[decided]** A pre or post hook that throws **does not affect
  execution**. The engine catches and swallows it. It must never reach
  `exception-ch` or change eval's outcome.
  **[finding]** Consequences of the swallow rule:
  - if pre throws, its token is `nil` → post must tolerate a `nil` token;
  - a post that throws before doing its work loses that work → see "deliver
    in `finally`" below.
- **[decided]** Hooks **must not block**. They run inside core.async go blocks
  (lazy-scheduling, applicative core-async). `swap!` is fine; I/O is not. Put
  this in the protocol docstrings.
- Hooks are reentrant by necessity: they run concurrently (manifold futures,
  go blocks, sequence pipelines, an engine value shared across parallel evals).
  The token contract plus the state model below make that safe.

## State, merging and completeness (the `profile-eval` side)

- **[proposed]** State belongs to **one `profile-eval` call**, not the engine.
  `profile-eval` creates an atom, closes the hooks over it, and reads it after
  evaluation. Post does `swap! conj` of an event
  `{:event :path :start-ns :end-ns ...}`; the result is treated as a **bag**.
  Aggregation (totals per path, slowest subjects) is a pure fn over the bag,
  run afterwards. **[finding]** Of four merges tried against every arrival
  order, only conj-as-bag is both order-invariant and lossless. Assoc by path
  (what `nodely.profile` does today) is neither: a subject evaluated twice
  keeps only its last timing.
- **[decided]** Completeness is **`profile-eval`'s concern, not the
  engines'**. Pre registers a promise, post delivers it, and `profile-eval`
  waits on all registered promises before returning the bag.
  **[finding]** With post firing on every way user code ends (above), the
  only remaining way to hang is an async node that **never delivers** inside an
  eval that **aborted** for another reason. Plain eval returns because the
  abort wins, but that promise is never delivered. So `profile-eval` must
  **bound its wait on the error path** (a timeout, or return the partial bag
  with a count of pending subjects).
  **[finding]** `profile-eval`'s post must **deliver its promise in a
  `finally`**. Otherwise a bug in post, swallowed by the engine, leaves the
  promise undelivered and hangs `profile-eval`. A throwing pre registers no
  promise, so nothing hangs.
- **[decided]** The clock is a profiling concern, outside the hooks. Record one
  `(Instant/now)` and one `System/nanoTime` at the start of `profile-eval`;
  events carry `nanoTime`, converted to wall time afterwards.

## Answers to the `protocols.clj` comment-block questions

| Question | Answer |
|---|---|
| Installing twice: both, or overwrite? | Overwrite. One pre slot, one post slot; compose outside if ever needed. |
| If both, what order? | N/A at the engine. A composite controls its own order (pre in order, post in reverse). |
| Hooks for specific nodes only? | The hook receives event + path + node; `profile-eval` filters. Engines don't match. |
| What info does the hook get? | `subject` (event, path, node, index) for pre; plus token and result for post. **[proposed]** |
| Are hooks reentrant? | They must be. Concurrent by construction; the token contract + per-call bag make it safe. |

## Implementation errata

- Docstrings for `-pre-eval-hook` / `-post-eval-hook` should say "every
  **invocation of user code**", not "every single node". There are multiple
  events per sequence and none for `:value`. The protocol section near the top
  of this file ("six methods") becomes eight.
- Attach the sequence element index **before** dispatch: `pipeline-async`,
  `pipeline` and `deferred/future` don't preserve element identity at start.
- Decide whether `profile-eval` is synchronous only, or also gets a channel
  variant (`eval-key-channel`). The channel variant re-raises the completeness
  question for channel-returning evaluation.
- Duplicate events for the same subject are **expected**, and show redundant
  evaluation (which `nodely.profile`'s assoc by path hides today).
- `FnToChannel` is an open protocol. For user-extended types, use a delegating
  wrapper implementing both `IFn` and `FnToChannel`. For those, pre fires when
  `evaluation-channel` is called rather than inside the type's own execution
  context, which is in line with the accepted delivery error.
- Hook calls run inside go blocks: keep the hook-invocation wrapper itself
  non-blocking, and keep its try/catch (the swallow rule) from catching the
  user's own exception, which must still flow to `exception-ch`.
