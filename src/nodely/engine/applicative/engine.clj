(ns nodely.engine.applicative.engine
  (:require
   [nodely.engine.protocols :as engine.protocols]))

;; This namespace holds the single `ApplicativeEngine` type shared by every
;; applicative-family engine (:applicative.promesa, :applicative.core-async,
;; :applicative.virtual-future). Each engine is one instance of this type,
;; parameterized by (1) the fully-qualified symbol of its applicative "context"
;; -- the monad the whole applicative category is generalized over -- and (2)
;; its own availability gate.
;;
;; It is intentionally free of any compile-time dependency on the shared
;; implementation namespace `nodely.engine.applicative` (which requires
;; core.async) or on any optional applicative library (promesa,
;; virtual-future). Its only :require is `nodely.engine.protocols`. The shared
;; implementation is loaded lazily via `requiring-resolve` inside the method
;; bodies; each optional context library is loaded lazily by its gate delay and
;; by `-prepare-opts`, after `-enable-deref` has confirmed availability.

(def ^:private impl-ns 'nodely.engine.applicative)

(defn- impl
  "Lazily loads the shared applicative implementation namespace and resolves
  `fn-name` (a symbol) within it, returning the resolved var."
  [fn-name]
  (requiring-resolve (symbol (name impl-ns) (name fn-name))))

(def promesa-enable-deref
  "A delay yielding nil when the promesa applicative context can be loaded
  (i.e. promesa is on the classpath), or a failure map describing the missing
  dependency otherwise."
  (delay
   (try (require 'nodely.engine.applicative.promesa)
        nil
        (catch Exception e
          {:msg                   "Could not locate promesa on classpath."
           ::error                :missing-ns
           ::requested-namespaces '[nodely.engine.applicative.promesa]
           :cause                 e}))))

(def core-async-enable-deref
  "A delay yielding nil when the core.async applicative context can be loaded
  (i.e. core.async is on the classpath), or a failure map describing the
  missing dependency otherwise."
  (delay
   (try (require 'nodely.engine.applicative.core-async
                 'nodely.engine.core-async.core
                 'nodely.engine.core-async.iterative-scheduling
                 'nodely.engine.core-async.lazy-scheduling)
        nil
        (catch Exception e
          {:msg                   "Could not locate core-async on classpath."
           ::error                :missing-ns
           ::requested-namespaces '[nodely.engine.applicative.core-async
                                    nodely.engine.core-async.core
                                    nodely.engine.core-async.iterative-scheduling
                                    nodely.engine.core-async.lazy-scheduling]
           :cause                 e}))))

(def virtual-future-enable-deref
  "A delay yielding nil when the virtual-future applicative context can be
  loaded (i.e. the JVM is version 21+), or a failure map describing the missing
  dependency otherwise."
  (delay
   (try (import java.util.concurrent.ThreadPerTaskExecutor)
        (require 'nodely.engine.virtual-workers
                 'nodely.engine.applicative.virtual-future)
        nil
        (catch Exception e
          {:msg              "Classloader could not locate `java.util.concurrent.ThreadPerTaskExecutor`, virtual futures require JDK 21 or higher."
           ::error           :missing-class
           ::requested-class "java.util.concurrent.ThreadPerTaskExecutor"
           :cause            e}))))

(deftype ApplicativeEngine [context-sym enable-deref-delay]
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
    enable-deref-delay)

  (-prepare-opts [_engine opts]
    ;; Reproduces the old ::opts-fn: inject the applicative context. The keyword
    ;; is written as a fully-qualified literal so this namespace needs no alias
    ;; to `nodely.engine.applicative` (which would violate the "no optional dep
    ;; at load time" contract). requiring-resolve loads the context namespace
    ;; if needed; by the construct -> gate -> prepare-opts dispatch order the gate
    ;; has already loaded it, so this is effectively a resolve -- and using
    ;; requiring-resolve keeps -prepare-opts correct even if called before the
    ;; gate.
    (assoc opts :nodely.engine.applicative/context
           (var-get (requiring-resolve context-sym)))))

(defn ->promesa-applicative-engine
  "Returns an ApplicativeEngine for the promesa context."
  []
  (->ApplicativeEngine 'nodely.engine.applicative.promesa/context
                       promesa-enable-deref))

(defn ->core-async-applicative-engine
  "Returns an ApplicativeEngine for the core.async context."
  []
  (->ApplicativeEngine 'nodely.engine.applicative.core-async/context
                       core-async-enable-deref))

(defn ->virtual-future-applicative-engine
  "Returns an ApplicativeEngine for the virtual-future context."
  []
  (->ApplicativeEngine 'nodely.engine.applicative.virtual-future/context
                       virtual-future-enable-deref))
