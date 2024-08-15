(ns nodely.engine.applicative.promesa
  (:require [nodely.engine.applicative.protocols :as protocols]
            [promesa.core :as p]
            [promesa.protocols :as pp])
  (:import java.util.concurrent.CompletableFuture))

(declare context)

(extend-type CompletableFuture
     protocols/Contextual
     (-get-context [_] context)

     protocols/Extract
     (-extract [it]
       (try (deref it)
            (catch java.util.concurrent.ExecutionException e
              (throw (.getCause e))))))

(def ^:no-doc context
  (reify
    protocols/Context
    protocols/RunNode
    (-apply-fn  [_ f mv]
      (pp/-map mv f))
    protocols/Functor
    (-fmap [_ f mv]
      (pp/-map mv f))

    protocols/Monad
    (-mreturn [_ v]
      (pp/-promise v))

    (-mbind [_ mv f]
      (pp/-bind mv f))

    protocols/Applicative
    (-pure [_ v]
      (pp/-promise v))

    (-fapply [_ pf pv]
      (pp/-map (p/all [pf pv])
               (fn [[f v]]
                 (f v))))))
