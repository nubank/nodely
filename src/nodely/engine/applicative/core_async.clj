(ns nodely.engine.applicative.core-async
  (:require
   [clojure.core.async :as async]
   [clojure.core.async.impl.channels :as impl]
   [nodely.data :as data]
   [nodely.engine.applicative.protocols :as protocols]
   [nodely.engine.core-async.core :as core-async.core])
  (:import
   [clojure.core.async.impl.channels ManyToManyChannel]))

(declare context)

(defn nil-guard
  [it]
  (if (nil? it)
    ::nil
    it))

(defn nil-unguard
  [it]
  (if (= it ::nil)
    nil
    it))

(defn handle-read-value
  [v]
  (if (instance? Throwable v)
    (throw v)
    (nil-unguard v)))

(defmacro <?
  "Just like a <! macro, but if the value taken from the channel is a Throwable instance, it will throw it.
  <! takes a val from port. Must be called inside a (go ...) or (go-try ...) block.
  Will return nil if closed. Will park if nothing is available."
  [ch]
  `(handle-read-value (async/<! ~ch)))

(extend-type ManyToManyChannel
  protocols/Contextual
  (-get-context [_] context)

  protocols/Extract
  (-extract [it] (handle-read-value (async/<!! it))))

(defn promise-of
  [v]
  (let [ret (async/promise-chan)]
    (async/put! ret (nil-guard v))
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
     (async/go (let [retv# (try (nil-guard ~@body)
                                (catch Throwable t#
                                  t#))]
                 (async/>! ret# retv#)))
     ret#))

(def ^:no-doc context
  (reify
    protocols/Context
    protocols/RunNode
    (-apply-fn [_ f mv]
      (let [tags (::data/tags (meta f))]
        (cond (instance? nodely.engine.core_async.core.AsyncThunk f)
              (go-future (let [in           (<? mv)
                               exception-ch (async/promise-chan)
                               result-ch    (core-async.core/evaluation-channel f in {:exception-ch exception-ch})
                               merged-chs   (async/merge [result-ch exception-ch])
                               result       (<? merged-chs)]
                           (::data/value result)))
              (contains? tags ::data/blocking)
              (go-future (let [v (<? mv)]
                           (async/thread (f v))))
              :else
              (go-future (let [v (<? mv)]
                           (f v))))))
    protocols/Functor
    (-fmap [mn f mv]
      (go-future (let [v (<? mv)]
                   (f v))))

    protocols/Monad
    (-mreturn [_ v]
      (promise-of v))

    ;; Channel a -> (a -> Channel b) -> Channel b
    (-mbind [mn mv f]
      (go-future (let [v (<? mv)]
                   (<? (f v)))))

    protocols/Applicative
    (-pure [ctx v] ;;good
      (promise-of v))

    ;; Returns only one v, doesn't behave promise-like
    (-fapply [_ pf pv]
      (go-future (let [f (<? pf)
                       v (<? pv)]
                   (f v))))))
