(ns nodely.engine.lazy-env)

(deftype LazySchedulingEnvironment [env delay-map opts]
  clojure.lang.ILookup
  (valAt [this k]
    (or (.valAt this k nil)
        (let [ex (ex-info "Missing key on env" {:key k})]
          (when-let [exception-fn (::exception-fn opts)]
            (exception-fn this k ex))
          (throw ex))))
  (valAt [this k not-found]
    (if (find env k)
      @(get delay-map k)
      not-found)))

(defn scheduled-nodes
  "Return the current map of nodes to applicative contexts in the
  LazySchedulingEnvironment `lazy-env`."
  [lazy-env]
  (reduce-kv (fn [m k v]
               (if (realized? v)
                 (assoc m k @v)
                 m))
             {}
             (.-delay-map lazy-env)))

(defn lazy-env
  [env eval-fn opts]
  (let [this-promise (promise)
        delay-map (reduce-kv (fn [m k _]
                               (assoc m k (delay (eval-fn env k @this-promise opts))))
                             {}
                             env)
        lookup (->LazySchedulingEnvironment env delay-map opts)]
    (deliver this-promise lookup)
    lookup))
