(ns nodely.engine.async.manifold-engine
  (:require
   [nodely.engine.protocols :as engine.protocols]))

(def ^:private impl-ns 'nodely.engine.manifold)

(def enable-deref
  "A delay yielding nil when the manifold implementation can be loaded, or a
  failure map describing the missing dependency otherwise."
  (delay
   (try
     (require impl-ns)
     nil
     (catch Exception e
       {:msg                   "Could not locate manifold on classpath."
        ::error                :missing-ns
        ::requested-namespaces [impl-ns]
        :cause                 e}))))

(defn- impl
  "Lazily loads the implementation namespace and resolves `fn-name` within it."
  [fn-name]
  (requiring-resolve (symbol (name impl-ns) (name fn-name))))

(deftype AsyncManifoldEngine []
  engine.protocols/Engine
  (-eval [_engine env k _opts]
    ((impl 'eval) env k))

  (-eval-key [_engine env k _opts]
    ((impl 'eval-key) env k))

  (-eval-key-channel [_engine _env _k _opts]
    (throw (UnsupportedOperationException.
            "Engine :async.manifold does not support eval-key-channel.")))

  (-eval-key-channel-supported? [_engine]
    false)

  (-enable-deref [_engine]
    enable-deref)

  (-prepare-opts [_engine _opts]
    nil))
