(ns nodely.engine.applicative.virtual-future
  (:require
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
  protocols/Contextual
  (-get-context [_] context)

  protocols/Extract
  (-extract [it]
    (deref-unwrapped it)))

(def context
  (reify
    protocols/Context
    protocols/RunNode
    (-apply-fn  [_ f mv]
      (vfuture (f (deref-unwrapped mv))))

    protocols/Functor
    (-fmap [mn f mv]
      (vfuture (f (deref-unwrapped mv))))

    protocols/Monad
    (-mreturn [_ v]
      (vfuture v))

    (-mbind [mn mv f]
      (vfuture (let [v (deref-unwrapped mv)]
                 (deref-unwrapped (f v)))))

    protocols/Applicative
    (-pure [_ v]
      (vfuture v))

    (-fapply [_ pf pv]
      (vfuture (let [f (deref-unwrapped pf)
                     v (deref-unwrapped pv)]
                 (f v))))))
