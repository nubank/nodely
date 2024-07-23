(ns nodely.engine.applicative.promesa
  (:require
   [cats.protocols :as mp]
   [nodely.engine.applicative.protocols :as protocols]
   [promesa.core :as p]
   #?(:cljs [promesa.impl :as pi]))
  #?(:clj
     (:import
      [java.util.concurrent CompletableFuture CompletionStage])))

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

(declare as-cf)

(defn- unwrap-binding*
  [sym]
  [sym (list as-cf sym)])

(defprotocol ACFWrappable
  (-wrap [o] "Ensures `o` is in an ApplicativeCompletableFuture"))

(defmacro of-acf
  [& args]
  (if (vector? (first args))
    (let [[unwrap-syms & body] args]
      `(let [~@(mapcat unwrap-binding* unwrap-syms)]
         (do ~@body)))
    `(do ~@args)))

(defmacro acf
  [& args]
  `(-wrap (of-acf ~@args)))

(deftype ApplicativeCompletableFuture [^CompletableFuture completable-future]
  mp/Contextual
  (-get-context [_] context)

  mp/Extract
  (-extract [_]
    (try (deref completable-future)
         (catch java.util.concurrent.ExecutionException e
           (throw (.getCause e)))))

  CompletionStage
  (acceptEither [_ other action] (acf (.acceptEither completable-future other action)))
  (acceptEitherAsync [_ other action] (acf (.acceptEitherAsync completable-future other action)))
  (acceptEitherAsync [_ other action exec] (acf (.acceptEitherAsync completable-future other action exec)))
  (applyToEither [_ other fn] (acf (.applyToEither completable-future other fn)))
  (applyToEitherAsync [_ other fn] (acf (.applyToEitherAsync completable-future other fn)))
  (applyToEitherAsync [_ other fn exec] (acf (.applyToEitherAsync completable-future other fn exec)))
  (exceptionally [_ fn] (acf (.exceptionally completable-future fn)))
  (handle [_ fn] (acf (.handle completable-future fn)))
  (handleAsync [_ fn] (acf (.handleAsync completable-future fn)))
  (handleAsync [_ fn exec] (acf (.handleAsync completable-future fn exec)))
  (runAfterBoth [_ other action] (acf (.runAfterBoth completable-future other action)))
  (runAfterBothAsync [_ other action] (acf (.runAfterBothAsync completable-future other action)))
  (runAfterBothAsync [_ other action exec] (acf (.runAfterBothAsync completable-future other action exec)))
  (runAfterEither [_ other action] (acf (.runAfterEither completable-future other action)))
  (runAfterEitherAsync [_ other action] (acf (.runAfterEitherAsync completable-future other action)))
  (runAfterEitherAsync [_ other action exec] (acf (.runAfterEitherAsync completable-future other action exec)))
  (thenAccept [_ action] (acf (.thenAccept completable-future action)))
  (thenAcceptAsync [_ action] (acf (.thenAcceptAsync completable-future action)))
  (thenAcceptAsync [_ action exec] (acf (.thenAcceptAsync completable-future action exec)))
  (thenAcceptBoth [_ other action] (acf (.thenAcceptBoth completable-future other action)))
  (thenAcceptBothAsync [_ other action] (acf (.thenAcceptBothAsync completable-future other action)))
  (thenAcceptBothAsync [_ other action exec] (acf (.thenAcceptBothAsync completable-future other action exec)))
  (thenApply [_ fn] (acf (.thenApply completable-future fn)))
  (thenApplyAsync [_ fn] (acf (.thenApplyAsync completable-future fn)))
  (thenApplyAsync [_ fn exec] (acf (.thenApplyAsync completable-future fn exec)))
  (thenCombine [_ other fn] (acf (.thenCombine completable-future other fn)))
  (thenCombineAsync [_ other fn] (acf (.thenCombineAsync completable-future other fn)))
  (thenCombineAsync [_ other fn exec] (acf (.thenCombineAsync completable-future other fn exec)))
  (thenCompose [_ fn] (acf (.thenCompose completable-future fn)))
  (thenComposeAsync [_ fn] (acf (.thenComposeAsync completable-future fn)))
  (thenComposeAsync [_ fn exec] (acf (.thenComposeAsync completable-future fn exec)))
  (thenRun [_ action] (acf (.thenRun completable-future action)))
  (thenRunAsync [_ action] (acf (.thenRunAsync completable-future action)))
  (thenRunAsync [_ action exec] (acf (.thenRunAsync completable-future action exec)))
  (toCompletableFuture [_] (.toCompletableFuture completable-future))
  (whenComplete [_ action] (acf (.whenComplete completable-future action)))
  (whenCompleteAsync [_ action] (acf (.whenCompleteAsync completable-future action)))
  (whenCompleteAsync [_ action exec] (acf (.whenCompleteAsync completable-future action exec))))

(defn- as-cf
  [^ApplicativeCompletableFuture acf]
  (.-completable-future acf))

(extend-protocol ACFWrappable
  Object
  (-wrap [o] (ApplicativeCompletableFuture. (p/promise o)))

  nil
  (-wrap [_] (ApplicativeCompletableFuture. (p/promise nil)))

  CompletableFuture
  (-wrap [cf] (ApplicativeCompletableFuture. cf))

  ApplicativeCompletableFuture
  (-wrap [acf] acf))

(def ^:no-doc context
  (reify
    mp/Context
    protocols/RunNode
    (-apply-fn  [_ f mv]
      (acf [mv] (p/map f mv)))
    mp/Functor
    (-fmap [mn f mv]
      (acf [mv] (p/map f mv)))

    mp/Monad
    (-mreturn [_ v]
      (acf [] v))

    (-mbind [mn mv f]
      (acf [mv]
           (p/bind mv (comp p/promise f))))

    mp/Applicative
    (-pure [_ v]
      (acf [] v))

    (-fapply [_ pf pv]
      (acf [pf pv]
        (p/map (fn [[f v]]
                 (f v))
               (p/all [pf pv]))))))
