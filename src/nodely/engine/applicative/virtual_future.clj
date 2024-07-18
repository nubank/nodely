(ns nodely.engine.applicative.virtual-future
  (:require
   [cats.protocols :as mp]
   [nodely.engine.applicative.protocols :as protocols]
   [nodely.engine.virtual-future :as virtual-future :refer [vfuture]])
  (:import
   nodely.engine.virtual_future.GreenFuture))

(declare context)

(defn deref-unwrapped
  [it]
  (try (deref it)
       (catch java.util.concurrent.ExecutionException e
         (throw (.getCause e)))))

(extend-type GreenFuture
  mp/Contextual
  (-get-context [_] context)

  mp/Extract
  (-extract [it]
    (deref-unwrapped it)))

(def context
  (reify
    mp/Context
    protocols/RunNode
    (-apply-fn  [_ f mv]
      (vfuture (f (deref-unwrapped mv))))

    mp/Functor
    (-fmap [mn f mv]
      (vfuture (f (deref-unwrapped mv))))

    mp/Monad
    (-mreturn [_ v]
      (vfuture v))

    (-mbind [mn mv f]
      (vfuture (let [v (deref-unwrapped mv)]
                 (deref-unwrapped (f v)))))

    mp/Applicative
    (-pure [_ v]
      (vfuture v))

    (-fapply [_ pf pv]
      (vfuture (let [f (deref-unwrapped pf)
                     v (deref-unwrapped pv)]
                 (f v))))))
