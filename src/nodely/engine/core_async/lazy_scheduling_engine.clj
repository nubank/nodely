(ns nodely.engine.core-async.lazy-scheduling-engine
  (:require
   [nodely.engine.protocols :as engine.protocols]))

;; This namespace is intentionally free of any compile-time dependency on
;; `clojure.core.async` so that `CoreAsyncLazySchedulingEngine` can be
;; constructed -- and asked, via `-enable-deref`, whether it can run -- even
;; when core.async is absent from the classpath. The engine does no core.async
;; work until an `-eval*` method is called; the implementation namespace (which
;; does require core.async) is loaded lazily at that point, and by
;; `-enable-deref`.

(def ^:private impl-ns 'nodely.engine.core-async.lazy-scheduling)

(def enable-deref
  "A delay yielding nil when the core.async lazy-scheduling implementation can
  be loaded (i.e. core.async is on the classpath), or a failure map describing
  the missing dependency otherwise."
  (delay
   (try
     (require impl-ns)
     nil
     (catch Exception e
       {:msg                   "Could not locate core-async on classpath."
        ::error                :missing-ns
        ::requested-namespaces [impl-ns]
        :cause                 e}))))

(defn- impl
  "Lazily loads the implementation namespace and resolves `fn-name` within it."
  [fn-name]
  (requiring-resolve (symbol (name impl-ns) (name fn-name))))

(deftype CoreAsyncLazySchedulingEngine []
  engine.protocols/Engine
  (-eval [_engine env k opts]
    ((impl 'eval) env k opts))

  (-eval-key [_engine env k opts]
    ((impl 'eval-key) env k opts))

  (-eval-key-channel [_engine env k opts]
    ((impl 'eval-key-channel) env k opts))

  (-eval-key-channel-supported? [_engine]
    true)

  (-enable-deref [_engine]
    enable-deref)

  (-prepare-opts [_engine opts]
    opts))
