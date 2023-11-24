(ns nodely.engine.applicative.promesa
  (:require
   [cats.protocols :as mp]
   [nodely.engine.applicative.protocols :as protocols]
   [promesa.core :as p]
   [promesa.protocols :as pp]
   #?(:cljs [promesa.impl :as pi]))
  #?(:clj
     (:import
      java.util.concurrent.CompletableFuture)))

(declare context)

#?(:cljs
   (defn extend-promise!
     [t]
     (extend-type t
       mp/Contextual
       (-get-context [_] context)

       mp/Extract
       (-extract [it]
         (pp/-extract it)))))

#?(:cljs (extend-promise! pi/*default-promise*))

#?(:clj
   (extend-type CompletableFuture
     mp/Contextual
     (-get-context [_] context)

     mp/Extract
     (-extract [it]
       (try (deref it)
            (catch java.util.concurrent.ExecutionException e
              (throw (.getCause e)))))))

(def ^:no-doc context
  (reify
    mp/Context
    protocols/RunNode
    (-apply-fn  [_ f mv]
      (pp/-map mv f))
    mp/Functor
    (-fmap [mn f mv]
      (pp/-map mv f))

    mp/Monad
    (-mreturn [_ v]
      (pp/-promise v))

    (-mbind [mn mv f]
      (pp/-bind mv f))

    mp/Applicative
    (-pure [_ v]
      (pp/-promise v))

    (-fapply [_ pf pv]
      (pp/-map (p/all [pf pv])
               (fn [[f v]]
                 (f v))))))
