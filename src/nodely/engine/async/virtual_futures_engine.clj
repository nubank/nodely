(ns nodely.engine.async.virtual-futures-engine
  (:require
   [nodely.engine.protocols :as engine.protocols]))

(def ^:private impl-ns 'nodely.engine.virtual-workers)

(def enable-deref
  "A delay yielding nil when virtual futures can be loaded, or a failure map
  describing the missing JDK support otherwise."
  (delay
   (try
     (import java.util.concurrent.ThreadPerTaskExecutor)
     (require impl-ns)
     nil
     (catch Exception e
       {:msg              "Classloader could not locate `java.util.concurrent.ThreadPerTaskExecutor`, virtual futures require JDK 21 or higher."
        ::error           :missing-class
        ::requested-class "java.util.concurrent.ThreadPerTaskExecutor"
        :cause            e}))))

(defn- impl
  "Lazily loads the implementation namespace and resolves `fn-name` within it."
  [fn-name]
  (requiring-resolve (symbol (name impl-ns) (name fn-name))))

(deftype AsyncVirtualFuturesEngine []
  engine.protocols/Engine
  (-eval [_engine env k _opts]
    ((impl 'eval) env k))

  (-eval-key [_engine env k _opts]
    ((impl 'eval-key) env k))

  (-eval-key-channel [_engine env k _opts]
    ((impl 'eval-key-channel) env k))

  (-eval-key-channel-supported? [_engine]
    true)

  (-enable-deref [_engine]
    enable-deref)

  (-prepare-opts [_engine _opts]
    nil))
