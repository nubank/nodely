(ns nodely.engine.applicative.core-async
  (:require
   [cats.protocols :as mp]
   [clojure.core.async :as async]
   [nodely.data :as data]
   [clojure.core.async.impl.channels :as impl])
  (:import
   [clojure.core.async.impl.channels ManyToManyChannel]))

(declare context)

(defn handle-value
  [v]
  (if (instance? Throwable v)
    (throw v)
    (::data/value v)))

(defmacro <?
  "Just like a <! macro, but if the value taken from the channel is a Throwable instance, it will throw it.
  <! takes a val from port. Must be called inside a (go ...) or (go-try ...) block.
  Will return nil if closed. Will park if nothing is available."
  [ch]
  `(handle-value (async/<! ~ch)))

(extend-type ManyToManyChannel
  mp/Contextual
  (-get-context [_] context)

  mp/Extract
  (-extract [it] (handle-value #nu/tapd (async/<!! it))))

(defn promise-of
  [v]
  (let [ret (async/promise-chan)]
    (async/put! ret (data/value v))
    ret))

(defmacro go-future
  "Executes `body` in a core-async worker, with additional interesting
  caveats compared with `go`:

  - The body is assumed to execute once,
  reading promise-chan core.async channels.

  - The result of executing body will be born on the promise channel
  returned by go-future

  - If the body throws an uncaught excpetion, then that exception will
  be born on the promise channel returned by go-future

  - If a channel read attempted in body returns an exception, that
  exception will be immediately thrown."
  [& body]
  `(let [ret# (async/promise-chan)]
     (async/go (let [retv# (try (data/value ~@body)
                                (catch Throwable t#
                                  t#))]
                 (async/>! ret# retv#)))
     ret#))

;; WIP Let's try abstracting the pattern we've been repeatedly doing
;; BUT with error handling
#_(defmacro go-promise
    [& body]
    (async/go (try ~@body)))

(def ^:no-doc context
  (reify
    mp/Context
    mp/Functor
    (-fmap [mn f mv]
      (go-future (let [v (<? mv)]
                   (f v))))

    mp/Monad
    (-mreturn [_ v]
      (promise-of v))

    ;; Channel a -> (a -> Channel b) -> Channel b
    (-mbind [mn mv f]
      (go-future (let [v (<? mv)]
                   (<? (f v)))))

    mp/Applicative
    (-pure [ctx v] ;;good
      (promise-of v))

    ;; Returns only one v, doesn't behave promise-like

    (-fapply [_ pf pv] ;; good??
      (go-future (let [f (<? pf)
                       v (<? pv)]
                   (f v))))))
