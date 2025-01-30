(ns nodely.engine.applicative.manifold
  (:require
   [manifold.deferred :as deferred]
   [nodely.engine.applicative.protocols :as protocols]))

(declare context)

(defn deref-unwrapped
  [it]
  (try (deref it)
       (catch java.util.concurrent.ExecutionException e
         (throw (.getCause e)))))

(extend-type manifold.deferred.Deferred
  protocols/Contextual
  (-get-context [_] context)

  protocols/Extract
  (-extract [it]
    (deref-unwrapped it)))

(def context
  (reify
    protocols/RunNode
    (-apply-fn  [_ f mv]
      (deferred/future (f (deref-unwrapped mv))))

    protocols/Functor
    (-fmap [_ f mv]
      (deferred/future (f (deref-unwrapped mv))))

    protocols/Monad
    (-mreturn [_ v]
      (deferred/future v))

    (-mbind [_ mv f]
      (deferred/future (let [v (deref-unwrapped mv)]
                         (deref-unwrapped (f v)))))

    protocols/Applicative
    (-pure [_ v]
      (deferred/future v))

    (-fapply [_ pf pv]
      (deferred/future (let [f (deref-unwrapped pf)
                             v (deref-unwrapped pv)]
                         (f v))))))
