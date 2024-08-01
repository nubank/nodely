(ns nodely.engine.applicative.protocols
  (:require
   [cats.context :as context]))

(defprotocol RunNode
  (-apply-fn [ctx f mv]))

(defn apply-fn
  [f mv]
  (let [ctx (context/infer mv)]
    (-apply-fn ctx f mv)))

(defprotocol Functor
  "A data type that can be mapped over without altering its context."
  (-fmap [ftor f fv] "Applies function f to the value(s) inside the context of the functor fv."))

(defprotocol Applicative
  "The Applicative abstraction."
  (-fapply [app af av]
    "Applies the function(s) inside af's context to the value(s)
     inside av's context while preserving the context.")
  (-pure [app v]
    "Takes any context or monadic value `app` and any value `v`, and puts
     the value `v` in the most minimal context (normally `mempty`) of same type of `app`"))

(defprotocol Monad
  "The Monad abstraction."
  (-mreturn [m v])
  (-mbind [m mv f]))

(defprotocol Context
  "A marker protocol for identifying the valid context types.")

(defprotocol Contextual
  "Abstraction that establishes a concrete type as a member of a context.

  A great example is the Maybe monad type Just. It implements
  this abstraction to establish that Just is part of
  the Maybe monad."
  (-get-context [_] "Get the context associated with the type."))
