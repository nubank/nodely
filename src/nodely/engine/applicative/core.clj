(ns nodely.engine.applicative.core
  (:require
   [clojure.set :as set]
   [nodely.engine.applicative.protocols :as p]))

(defn throw-illegal-argument
  {:no-doc true :internal true}
  [^String text]
  (throw (IllegalArgumentException. text)))

;; CONTEXT STUFF HERE

(defn context?
  "Return `true` if the provided value satisfies
  the Context protocol."
  [v]
  (satisfies? p/Context v))

(defprotocol Contextual
  "Abstraction that establishes a concrete type as a member of a context.

  A great example is the Maybe monad type Just. It implements
  this abstraction to establish that Just is part of
  the Maybe monad."
  (-get-context [_] "Get the context associated with the type."))

(extend-protocol p/Contextual
  java.lang.Object
  (-get-context [_] nil))

(defn infer
  "Given an optional value infer its context. If context is already set, it
  is returned as is without any inference operation."
  {:no-doc true}
  [v]
  (if-let [context (p/-get-context v)]
    context
    (throw-illegal-argument
      (str "No context is set and it can not be automatically "
           "resolved from provided value"))))

;; END CONTEXT STUFF

;; FUNCTOR STUFF
(defn fmap
  "Apply a function `f` to the value wrapped in functor `fv`,
  preserving the context type."
  [f fv]
  (let [ctx (infer fv)]
    (p/-fmap ctx f fv)))

;; MONAD STUFF
(defn bind
  "Given a monadic value `mv` and a function `f`,
  apply `f` to the unwrapped value of `mv`.

      (bind (either/right 1) (fn [v]
                               (return (inc v))))
      ;; => #<Right [2]>

  For convenience, you may prefer to use the `mlet` macro,
  which provides a beautiful, `let`-like syntax for
  composing operations with the `bind` function."
  [mv f]
  (let [ctx (infer mv)]
    (p/-mbind ctx mv f)))

(defn return
  "This is a monad version of `pure` and works
  identically to it."
  [ctx v]
  (p/-mreturn ctx v))

(defn join
  "Remove one level of monadic structure.
  This is the same as `(bind mv identity)`."
  [mv]
  (bind mv identity))

(defmacro mlet
     "Monad composition macro that works like Clojure's
     `let`. This facilitates much easier composition of
     monadic computations.

     Let's see an example to understand how it works.
     This code uses bind to compose a few operations:

         (bind (just 1)
               (fn [a]
                 (bind (just (inc a))
                         (fn [b]
                           (return (* b 2))))))
         ;=> #<Just [4]>

     Now see how this code can be made clearer
     by using the mlet macro:

         (mlet [a (just 1)
                b (just (inc a))]
           (return (* b 2)))
         ;=> #<Just [4]>
     "
     [bindings & body]
     (when-not (and (vector? bindings)
                    (not-empty bindings)
                    (even? (count bindings)))
       (throw (IllegalArgumentException. "bindings has to be a vector with even number of elements.")))
     (->> (reverse (partition 2 bindings))
          (reduce (fn [acc [l r]] `(bind ~r (fn [~l] ~acc)))
                  `(do ~@body))))

;; APPLICATIVE STUFF
(defn pure
  "Given any value `v`, return it wrapped in
  the default/effect-free context.

  This is a multi-arity function that with arity `pure/1`
  uses the dynamic scope to resolve the current
  context. With `pure/2`, you can force a specific context
  value.

  Example:

      (with-context either/context
        (pure 1))
      ;; => #<Right [1]>

      (pure either/context 1)
      ;; => #<Right [1]>
  "
  [ctx v]
  (p/-pure ctx v))

(defn fapply
  "Given a function wrapped in a monadic context `af`,
  and a value wrapped in a monadic context `av`,
  apply the unwrapped function to the unwrapped value
  and return the result, wrapped in the same context as `av`.

  This function is variadic, so it can be used like
  a Haskell-style left-associative fapply."
  [af & avs]
  {:pre [(seq avs)]}
  (let [ctx (infer af)]
    (reduce (partial p/-fapply ctx) af avs)))

(defn- deps
  [expr syms]
  (cond
    (and (symbol? expr)
         (contains? syms expr))
    (list expr)

    (seq? expr)
    (mapcat #(deps % syms) expr)

    :else
    '()))

(defn- rename-sym
  [expr renames]
  (get renames expr expr))

(defn- rename
  [expr renames]
  (cond
    (symbol? expr)
    (rename-sym expr renames)
    (seq? expr)
    (map #(rename % renames) expr)
    :else
    expr))

(defn- dedupe-symbols*
  [sym->ap body]
  (letfn [(renamer [{:keys [body syms aps seen renames] :as summ} [s ap]]
           (let [ap' (rename ap renames)
                 new-aps (conj aps ap')]
             (if (seen s)
               (let [s' (gensym)
                     new-syms (conj syms s')
                     new-seen (conj seen s')
                     new-renames (assoc renames s s')
                     new-body (rename body new-renames)]
                 {:syms new-syms
                  :aps new-aps
                  :seen new-seen
                  :renames new-renames
                  :body new-body})
               (let [new-syms (conj syms s)
                     new-seen (conj seen s)]
                 {:syms new-syms
                  :aps new-aps
                  :seen new-seen
                  :renames renames
                  :body body}))))]
    (let [summ
          (reduce renamer
                  {:syms []
                   :aps []
                   :seen #{}
                   :renames {}
                   :body body}
                  sym->ap)]
      [(mapv vector (:syms summ) (:aps summ)) (:body summ)])))

(defn- dedupe-symbols
  [bindings body]
  (let [syms (map first bindings)
        aps (map second bindings)
        sym->ap (mapv vector syms aps)]
    (dedupe-symbols* sym->ap body)))

(defn- dependency-map
  [sym->ap]
  (let [syms (map first sym->ap)
        symset (set syms)]
    (into []
          (clojure.core/for [[s ap] sym->ap
                :let [ds (set (deps ap symset))]]
            [s ds]))))

(defn- remove-deps
  [deps symset]
  (let [removed (clojure.core/for [[s depset] deps]
                  [s (set/difference depset symset)])]
    (into (empty deps) removed)))

(defn- topo-sort*
  [deps seen batches current]
  (if (empty? deps)
    (conj batches current)
    (let [dep (first deps)
          [s dependencies] dep
          dependant? (some dependencies seen)]
      (if (nil? dependant?)
        (recur (subvec deps 1)
               (conj seen s)
               batches
               (conj current s))
        (recur (remove-deps (subvec deps 1) (set current))
               (conj seen s)
               (conj batches current)
               [s])))))

(defn- topo-sort
  [deps]
  (let [syms (into #{} (map first deps))]
    (topo-sort* deps #{} [] [])))

(defn- bindings->batches
  [bindings]
  (let [syms (map first bindings)
        aps (map second bindings)
        sym->ap (mapv vector syms aps)
        sorted-deps (topo-sort (dependency-map sym->ap))]
    sorted-deps))

(defn- alet*
  [batches env body]
  (let [fb (first batches)
        rb (rest batches)
        fs (first fb)
        fa (get env fs)
        code
        (reduce (fn [acc syms]
                  (let [fs (first syms)
                        fa (get env fs)
                        rs (rest syms)
                        faps (map #(get env %) rs)]
                    (if (= (count syms) 1)
                      `(fmap (fn [~fs] ~acc) ~fa)
                      (let [cf (reduce (fn [f sym] `(fn [~sym] ~f))
                                       acc
                                       (reverse syms))]
                        `(fapply (fmap ~cf ~fa) ~@faps)))))
                `(do ~@body)
                (reverse batches))
        join-count (dec (count batches))]
    (reduce (fn [acc _]
            `(join ~acc))
        code
        (range join-count))))

(defmacro alet
     "Applicative composition macro similar to Clojure's
     `let`. This macro facilitates composition of applicative
     computations using `fmap` and `fapply` and evaluating
     applicative values in parallel.

     Let's see an example to understand how it works.
     This code uses fmap for executing computations inside
     an applicative context:

       (fmap (fn [a] (inc a)) (just 1))
       ;=> #<Just [2]>

     Now see how this code can be made clearer
     by using the alet macro:

       (alet [a (just 1)]
         (inc a))
       ;=> #<Just [2]>

     Let's look at a more complex example, imagine we have
     dependencies between applicative values:

       (join
         (fapply
          (fmap
            (fn [a]
              (fn [b]
                (fmap (fn [c] (inc c))
                      (just (+ a b)))))
            (just 1))
          (just 2)))
       ;=> #<Just [4]>

     This is greatly simplified using `alet`:

       (alet [a (just 1)
              b (just 2)
              c (just (+ a b))]
         (inc c))
      ;=> #<Just [4]>

     The intent of the code is much clearer and evaluates `a` and `b`
     at the same time, then proceeds to evaluate `c` when all the values
     it depends on are available. This evaluation strategy is specially
     helpful for asynchronous applicatives."
     [bindings & body]
     (when-not (and (vector? bindings)
                    (not-empty bindings)
                    (even? (count bindings)))
       (throw (IllegalArgumentException. "bindings has to be a vector with even number of elements.")))
     (let [bindings (partition 2 bindings)
           [bindings body] (dedupe-symbols bindings body)
           batches (bindings->batches bindings)
           env (into {} bindings)]
       (if (and (= (count batches) 1)
                (= (count (map first bindings)) 1))
         `(fmap (fn [~@(map first bindings)]
                  ~@body)
                ~@(map second bindings))
         (alet* batches env body))))
