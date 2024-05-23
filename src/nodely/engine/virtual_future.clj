(ns nodely.engine.virtual-future
  (:import
   [clojure.lang IBlockingDeref IDeref IPending]
   [java.util.concurrent ExecutorService Executors Future]))

(def ^:private virtual-thread-factory
  (-> (Thread/ofVirtual)
      (.name "nodely-virtual-thread-" 0)
      .factory))

(def executor-service (Executors/newThreadPerTaskExecutor virtual-thread-factory))

(deftype GreenFuture [^Future executor-future]
  IDeref
  (deref [_] (#'clojure.core/deref-future executor-future))
  IBlockingDeref
  (deref [_ timeout-ms timeout-val]
    (#'clojure.core/deref-future executor-future timeout-ms timeout-val))
  IPending
  (isRealized [_] (.isDone executor-future))
  Future
  (get [_] (.get executor-future))
  (get [_ timeout unit] (.get executor-future timeout unit))
  (isCancelled [_] (.isCancelled executor-future))
  (isDone [_] (.isDone executor-future))
  (cancel [_ interrupt?] (.cancel executor-future interrupt?)))

(defn future-submit
  "This function is similar to the clojure.core future-call. The only difference is that it receives as a param a dynamic
   executor-service instead of using clojure.lang.Agent/soloExecutor"
  [^ExecutorService executor-service f]
  (let [f (#'clojure.core/binding-conveyor-fn f)
        fut (.submit executor-service ^Callable f)]
    (GreenFuture. fut)))

(defmacro vfuture
  "Similar to clojure.core future macro. The only difference is that it calls future-submit"
  [& body] `(future-submit executor-service (^{:once true} fn* [] ~@body)))
