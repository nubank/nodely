(ns nodely.profile
  "Time profiling utilities for nodely environments.
   
   Provides non-invasive profiling that wraps existing environments
   to record execution timing for each node without modifying the
   core execution logic."
  (:refer-clojure :exclude [sequence])
  (:require
   [nodely.data :as data]))

;; Forward declaration for conditional core.async support
(declare make-profiled-async-thunk)

(defn- normalize-path
  "Normalizes a path for use as a key.
   Single-element paths become simple keywords, multi-element paths stay as vectors."
  [path]
  (if (= 1 (count path))
    (first path)
    path))

(defn- record-timing!
  "Records elapsed time in nanoseconds to the profile atom at the given path.
   Single-element paths are stored as keywords, multi-element as vectors."
  [profile-atom path elapsed-ns]
  (swap! profile-atom assoc (normalize-path path) {:elapsed-ns elapsed-ns}))

(defn- wrap-fn-with-timing
  "Wraps a function to record its execution time into the profile atom."
  [f profile-atom path]
  (fn [args]
    (let [start  (System/nanoTime)
          result (f args)
          end    (System/nanoTime)]
      (record-timing! profile-atom path (- end start))
      result)))

(defn- channel-leaf-fn?
  "Checks if a function is an AsyncThunk (channel-leaf)."
  [f]
  (and (record? f) (contains? f :channel-fn)))

(defn- profile-leaf
  "Wraps a leaf node to record execution time.
   Channel-leaf nodes (AsyncThunk) are wrapped specially to handle async timing."
  [leaf profile-atom path]
  (let [f (::data/fn leaf)]
    (if (channel-leaf-fn? f)
      (update leaf ::data/fn #(make-profiled-async-thunk % profile-atom path))
      (update leaf ::data/fn #(wrap-fn-with-timing % profile-atom path)))))

(defn- profile-sequence
  "Wraps a sequence node to record execution time of its process-node.
   Note: sequence nodes with value process-nodes (simple fns) cannot be profiled
   at the individual element level since the fn is applied by the engine."
  [sequence-node profile-atom path]
  (let [process-node (::data/process-node sequence-node)]
    (case (::data/type process-node)
      :value sequence-node ; value process-nodes are just functions stored as values
      :leaf  (update sequence-node ::data/process-node
                     #(profile-leaf % profile-atom (conj path :process))))))

(declare profile-node)

(defn- profile-branch
  "Wraps a branch node to record execution time of condition, truthy, and falsey paths."
  [branch profile-atom path]
  (-> branch
      (update ::data/condition #(profile-node % profile-atom (conj path :condition)))
      (update ::data/truthy    #(profile-node % profile-atom (conj path :truthy)))
      (update ::data/falsey    #(profile-node % profile-atom (conj path :falsey)))))

(defn profile-node
  "Recursively wraps a node for profiling.
   
   For branches, uses path-aware keys to distinguish condition/truthy/falsey.
   Values are not wrapped since they're immediate and require no computation."
  [node profile-atom path]
  (case (::data/type node)
    :value    node
    :leaf     (profile-leaf node profile-atom path)
    :sequence (profile-sequence node profile-atom path)
    :branch   (profile-branch node profile-atom path)))

(defn profile-env
  "Transforms an environment for profiling.
   
   Returns a tuple of [profiled-env profile-atom] where:
   - profiled-env: the environment with timing instrumentation
   - profile-atom: an atom that will contain timing data after evaluation
   
   Example usage:
   ```clojure
   (let [[profiled-env profile-data] (profile-env my-env)]
     (nodely/eval-key profiled-env :target {::nodely/engine :sync.lazy})
     @profile-data)
   ;; => {:a {:elapsed-ns 1000234}
   ;;     :b {:elapsed-ns 2003421}
   ;;     [:c :condition] {:elapsed-ns 500123}}
   ```
   
   The profile data uses vector paths for nested nodes:
   - Top-level leaf: :key -> {:elapsed-ns n}
   - Branch condition: [:key :condition] -> {:elapsed-ns n}
   - Branch truthy path: [:key :truthy] -> {:elapsed-ns n}
   - Nested branches: [:key :truthy :condition] -> {:elapsed-ns n}"
  [env]
  (let [profile-atom (atom {})]
    [(reduce-kv
      (fn [acc k node]
        (assoc acc k (profile-node node profile-atom [k])))
      {}
      env)
     profile-atom]))

(defn profile-env-with-atom
  "Like profile-env, but uses a provided atom for collecting profile data.
   
   Useful when you want to aggregate profiling data across multiple evaluations
   or provide your own storage mechanism."
  [env profile-atom]
  (reduce-kv
   (fn [acc k node]
     (assoc acc k (profile-node node profile-atom [k])))
   {}
   env))

(defn total-time
  "Calculates the total time from profile data.
   
   Note: This is the sum of all node times, which may be greater than
   wall-clock time if nodes were evaluated in parallel."
  [profile-data]
  (->> profile-data
       vals
       (map :elapsed-ns)
       (reduce + 0)))

(defn slowest-nodes
  "Returns the n slowest nodes from profile data, sorted by elapsed time descending.
   
   Each entry is a map with :path and :elapsed-ns keys."
  ([profile-data]
   (slowest-nodes profile-data 10))
  ([profile-data n]
   (->> profile-data
        (map (fn [[path timing]] {:path path :elapsed-ns (:elapsed-ns timing)}))
        (sort-by :elapsed-ns >)
        (take n))))

(defn format-timing
  "Formats elapsed nanoseconds as a human-readable string."
  [elapsed-ns]
  (cond
    (< elapsed-ns 1000)       (str elapsed-ns " ns")
    (< elapsed-ns 1000000)    (format "%.2f µs" (/ elapsed-ns 1000.0))
    (< elapsed-ns 1000000000) (format "%.2f ms" (/ elapsed-ns 1000000.0))
    :else                     (format "%.2f s" (/ elapsed-ns 1000000000.0))))

(defn summarize
  "Returns a human-readable summary of profile data."
  [profile-data]
  {:total-time   (format-timing (total-time profile-data))
   :node-count   (count profile-data)
   :slowest      (->> (slowest-nodes profile-data 5)
                      (mapv #(update % :elapsed-ns format-timing)))})

;;
;; Async profiling support (requires core.async)
;;

(defn- try-require-async
  "Attempts to require core.async namespaces. Returns true if successful."
  []
  (try
    (require 'clojure.core.async)
    (require 'nodely.engine.core-async.core)
    true
    (catch Exception _
      false)))

;; Define the ProfiledAsyncThunk record type when core.async is available
(when (try-require-async)
  (eval
   '(do
      (require '[clojure.core.async :as async])
      (require '[nodely.engine.core-async.core :as core-async])

      (defrecord ProfiledAsyncThunk [channel-fn profile-atom path]
        clojure.lang.IFn
        (invoke [_ args]
          (let [start  (System/nanoTime)
                result (async/<!! (channel-fn args))
                end    (System/nanoTime)]
            (nodely.profile/record-timing! profile-atom path (- end start))
            result))

        core-async/FnToChannel
        (evaluation-channel [_ args opts]
          (let [start   (System/nanoTime)
                orig-ch (channel-fn args)]
            (async/go
              (try
                (let [[val port] (async/alts! [(:exception-ch opts) orig-ch] :priority true)]
                  (if (= port (:exception-ch opts))
                    (throw (core-async/user-exception val))
                    (let [end (System/nanoTime)]
                      (cond
                        (instance? Throwable val)
                        (do (async/>! (:exception-ch opts) val)
                            (throw (core-async/user-exception val)))

                        (nil? val)
                        (let [ex (ex-info "channel closed unexpectedly" {:channel orig-ch})]
                          (async/>! (:exception-ch opts) ex)
                          (throw (core-async/user-exception ex)))

                        :else
                        (do (nodely.profile/record-timing! profile-atom path (- end start))
                            (nodely.data/value val))))))
                (catch clojure.lang.ExceptionInfo e
                  (when-not (core-async/user-exception? e)
                    (throw e)))))))))))

(defn make-profiled-async-thunk
  "Creates a profiled wrapper for an AsyncThunk that properly handles
   both sync (IFn) and async (FnToChannel) execution contexts."
  [async-thunk profile-atom path]
  (if (and (try-require-async)
           (resolve 'nodely.profile/->ProfiledAsyncThunk))
    (let [constructor (resolve 'nodely.profile/->ProfiledAsyncThunk)
          channel-fn  (:channel-fn async-thunk)]
      (constructor channel-fn profile-atom path))
    ;; Fallback - just wrap for sync timing
    (let [channel-fn (:channel-fn async-thunk)
          <!!-fn     (when (try-require-async)
                       @(resolve 'clojure.core.async/<!!))]
      (fn [args]
        (let [start  (System/nanoTime)
              result (if <!!-fn
                       (<!!-fn (channel-fn args))
                       (.invoke async-thunk args))
              end    (System/nanoTime)]
          (record-timing! profile-atom path (- end start))
          result)))))
