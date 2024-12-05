(ns nodely.engine.applicative.manifold
  (:require
   [nodely.engine.applicative.protocols :as mp]
   [manifold.deferred :as deferred]))

(declare context)

(extend-type manifold.deferred.Deferred
  mp/Contextual
  (-get-context [_] context)

  mp/Extract
  (-extract [it]
    (try (deref it)
         (catch java.util.concurrent.ExecutionException e
           (throw (.getCause e))))))

(extend-type manifold.deferred.LeakAwareDeferred
  mp/Contextual
  (-get-context [_] context)

  mp/Extract
  (-extract [it]
    (try (deref it)
         (catch java.util.concurrent.ExecutionException e
           (throw (.getCause e))))))

(extend-type manifold.deferred.SuccessDeferred
  mp/Contextual
  (-get-context [_] context)

  mp/Extract
  (-extract [it]
    (try (deref it)
         (catch java.util.concurrent.ExecutionException e
           (throw (.getCause e))))))

(extend-type manifold.deferred.ErrorDeferred
  mp/Contextual
  (-get-context [_] context)

  mp/Extract
  (-extract [it]
    (try (deref it)
         (catch java.util.concurrent.ExecutionException e
           (throw (.getCause e))))))

(def ^:no-doc context
  (reify
    mp/RunNode
    (-apply-fn [_ f mv] (deferred/chain' mv f))
    mp/Functor
    (-fmap [_ f mv] (deferred/chain' mv f))

    mp/Monad
    (-mreturn [_ v] (deferred/future v))

    (-mbind [_ mv f]
      (deferred/chain' mv f))

    mp/Applicative
    (-pure [_ v] (deferred/future v))

    (-fapply [_ pf pv]
      (deferred/chain' (deferred/zip' pf pv)
                      (fn [[f v]]
                        (f v))))))
