(ns nodely.engine.lazy-env)

(deftype LazySchedulingEnvironment [env ref-map eval-fn opts]
  clojure.lang.ILookup
  (valAt [this k]
    (or (.valAt this k nil)
        (let [ex (ex-info "Missing key on env" {:key k})]
          (when-let [exception-fn (::exception-fn opts)]
            (exception-fn this k ex))
          (throw ex))))
  (valAt [this k not-found]
    (if (find env k)
      (let [first-read @ref-map]
        (if (find first-read k)
          (get first-read k)
          (-> (dosync
               (let [second-read @ref-map]
                 (if-not (find second-read k)
                   (alter ref-map assoc k (eval-fn env k this opts))
                   second-read)))
              (get k))))
      not-found)))

(defn scheduled-nodes
  "Return the current map of nodes to applicative contexts in the
  LazySchedulingEnvironment `lazy-env`."
  [lazy-env]
  @(.-ref-map lazy-env))

(defn lazy-env
  [env eval-fn opts]
  (->LazySchedulingEnvironment env (ref {}) eval-fn opts))
