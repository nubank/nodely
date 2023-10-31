(ns nodely.engine.applicative.manifold
  (:require
   [cats.protocols :as mp]
   [manifold.deferred :as deferred]))

(declare context)

(extend-type manifold.deferred.SuccessDeferred
  mp/Contextual
  (-get-context [_] context)

  mp/Extract
  (-extract [it]
    (try (deref it)
         (catch java.util.concurrent.ExecutionException e
           (throw (.getCause e))))))

(extend-type manifold.deferred.ErrorDeferred
  mp/Extract
  (-extract [it]
    (try (deref it)
         (catch java.util.concurrent.ExecutionException e
           (throw (.getCause e))))))

(def ^:no-doc context
  (reify
    mp/Context
    mp/Functor
    (-fmap [_ f mv]
      (deferred/chain mv f))

    mp/Monad ;; goood
    (-mreturn [_ v]
      (deferred/success-deferred v))

    ;; Channel a -> (a -> Channel b) -> Channel b
    (-mbind [_ mv f]
      (deferred/chain mv (fn [v]
                           (f v))))

    mp/Applicative
    (-pure [_ v]
      (deferred/success-deferred v))

    (-fapply [_ pf pv]
      (deferred/chain (deferred/zip' pf pv)
                      (fn [[f v]]
                        (f v))))))
