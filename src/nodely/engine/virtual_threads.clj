(ns nodely.engine.virtual-threads
  (:import
   [clojure.lang IBlockingDeref IDeref IPending]
   [java.util.concurrent ExecutorService Executors Future]))

(def ^:private virtual-thread-factory
  (-> (Thread/ofVirtual)
      (.name "magilla-virtual-thread-" 0)
      .factory))

(def executor-service (Executors/newThreadPerTaskExecutor virtual-thread-factory))

(defn future-submit
  "This function is similar to the clojure.core future-call. The only difference is that it receives as a param a dynamic
   executor-service instead of using clojure.lang.Agent/soloExecutor"
  [^ExecutorService executor-service f]
  (let [f (#'clojure.core/binding-conveyor-fn f)
        fut (.submit executor-service ^Callable f)]
    (reify
      IDeref
      (deref [_] (#'clojure.core/deref-future fut))
      IBlockingDeref
      (deref
        [_ timeout-ms timeout-val]
        (#'clojure.core/deref-future fut timeout-ms timeout-val))
      IPending
      (isRealized [_] (.isDone fut))
      Future
      (get [_] (.get fut))
      (get [_ timeout unit] (.get fut timeout unit))
      (isCancelled [_] (.isCancelled fut))
      (isDone [_] (.isDone fut))
      (cancel [_ interrupt?] (.cancel fut interrupt?)))))

(defmacro vfuture
  "Similar to clojure.core future macro. The only difference is that it calls future-submit"
  [& body] `(future-submit executor-service (^{:once true} fn* [] ~@body)))
