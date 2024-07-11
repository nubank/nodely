(ns nodely.engine.core-async.core
  (:require
   [clojure.core.async :as async]
   [nodely.data :as data]
   [nodely.syntax :as syntax]))

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

(defmacro meander
  "Wraps user expressions and pipes any exception through ex-ch"
  [ex-ch & body]
  `(async/go (try ~@body (catch Throwable t# (async/>! ~ex-ch t#)))))

(defmacro meander-blocking
  "Wraps user expressions and pipes any exception through ex-ch, runs in
  the core.async 'thread' worker pool so that blocking operations
  aren't (as) problematic."
  [ex-ch & body]
  `(async/thread (try ~@body (catch Throwable t# (async/>!! ~ex-ch t#)))))

(defmacro feedback-try
  [ex-ch & body]
  `(try ~@body
        (catch Throwable t#
          (async/put! ~ex-ch t#)
          nil)))

(defn user-exception
  [exception]
  (ex-info "User code exception" {:type :user-code-exception} exception))

(defn user-exception?
  [exception]
  (= (:type (ex-data exception)) :user-code-exception))

(defmacro jog
  "Same as go block that ignores exceptions tagged as user exceptions.
   This prevents an exception we already captured on the exception channel
   to be printed several times"
  [& body]
  `(async/go (try ~@body
                  (catch clojure.lang.ExceptionInfo e#
                    (when-not (user-exception? e#)
                      (throw e#))))))

(defmacro <request
  "non-blocking take from channel
   short circuits in the case of an exception present on exception-ch"
  [ch exception-ch]
  `(let [[val# port#] (async/alts! [~exception-ch ~ch] :priority true)]
     (if (= port# ~exception-ch)
       (throw (user-exception val#))
       val#)))

(defmacro <user-request
  "Takes from user supplied channel. If an exception is on the exception-ch,
   throws back an user exception.
   Since we don't control the user supplied channel, we consider three failure modes:
   - user channel is using the exception-channel
   - user channel is piping exceptions in the channel itself
   - user channel is not treating exceptions, in this case the exception will close the channel
   and return nil"
  [ch exception-ch]
  `(let [[val# port#] (async/alts! [~exception-ch ~ch] :priority true)]
     (cond
       (= port# ~exception-ch) (throw (user-exception val#))
       (instance? Throwable val#) (do (async/>! ~exception-ch val#)
                                      (throw (user-exception val#)))
       (nil? val#) (let [ex# (ex-info "channel closed unexpectedly" {:channel ~ch})]
                     (async/>! ~exception-ch ex#)
                     (throw (user-exception ex#)))
       :else val#)))

(defmacro <demand
  "Blocking version of <request"
  [ch exception-ch]
  `(let [[val# port#] (async/alts!! [~exception-ch ~ch] :priority true)]
     (if (= port# ~exception-ch)
       (throw val#)
       val#)))

(defprotocol FnToChannel
  (evaluation-channel [the-fn args opts]
    "Returns a channel bearing the result of evaluating
     the `lazy-env` with `the-fn`, the :nodely.data/fn
     value of `node`"))

(extend-protocol FnToChannel
  clojure.lang.IFn
  (evaluation-channel [the-fn args opts]
    (if (:blocking opts)
      (meander-blocking (:exception-ch opts) (data/value (the-fn args)))
      (meander (:exception-ch opts) (data/value (the-fn args))))))

(defrecord AsyncThunk
           [channel-fn]
  clojure.lang.IFn
  (invoke [_ args]
    (async/<!! (channel-fn args)))

  FnToChannel
  (evaluation-channel [_ args opts]
    (jog (data/value
          (<user-request (channel-fn args) (:exception-ch opts))))))

(defn channel-leaf
  [keys fn]
  (data/leaf keys (->AsyncThunk fn)))

(defmacro >channel-leaf
  [expr]
  (let [symbols-to-be-replaced (#'syntax/question-mark-symbols expr)
        fn-expr                (#'syntax/fn-with-arg-map symbols-to-be-replaced expr)]
    (list `channel-leaf
          (mapv #'syntax/question-mark->keyword symbols-to-be-replaced)
          fn-expr)))
