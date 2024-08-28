(ns nodely.engine.applicative.core
  (:refer-clojure :exclude [sequence])
  (:require
   [nodely.engine.applicative.protocols :as p]))

(defn throw-illegal-argument
  {:no-doc true :internal true}
  [^String text]
  (throw (IllegalArgumentException. text)))

;; CONTEXT STUFF HERE

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
  (or
   (when (nil? v) v)
   (p/-get-context v)
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

(defn sequence
  "Given a collection of monadic values, collect
  their values in a seq returned in the monadic context.

      (require '[cats.context :as ctx]
               '[cats.monad.maybe :as maybe]
               '[cats.core :as m])

      (m/sequence [(maybe/just 2) (maybe/just 3)])
      ;; => #<Just [[2, 3]]>

      (m/sequence [(maybe/nothing) (maybe/just 3)])
      ;; => #<Nothing>

      (ctx/with-context maybe/context
        (m/sequence []))
      ;; => #<Just [()]>
  "
  [context mvs]
  (if (empty? mvs)
    (return context ())
    (reduce (fn [mvs mv]
              (mlet [v mv
                     vs mvs]
                    (return context (cons v vs))))
            (return context ())
            (reverse mvs))))

(defn apply-fn
  [f mv]
  (let [ctx (infer mv)]
    (p/-apply-fn ctx f mv)))
