(ns nodely.engine.applicative-test
  (:require
   [clojure.core.async :as async]
   [clojure.test :refer :all]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.properties :as prop]
   [criterium.core :as criterium]
   [matcher-combinators.matchers :as matchers]
   [matcher-combinators.test :refer [match?]]
   [nodely.data :as data]
   [nodely.engine.applicative :as applicative]
   [nodely.engine.applicative.core-async :as core-async]
   [nodely.engine.applicative.promesa :as promesa]
   [nodely.engine.applicative.manifold :as manifold]
   [nodely.engine.applicative.synchronous :as synchronous]
   [nodely.engine.core :as core]
   [nodely.engine.core-async.core :as nodely.async]
   [nodely.engine.schema :as schema]
   [nodely.fixtures :as fixtures]
   [nodely.syntax :as syntax :refer [>leaf >sequence >value]]
   [nodely.syntax.schema :refer [yielding-schema]]
   [promesa.core :as p]
   [schema.core :as s]))

(def test-env {:a (>value 2)
               :b (>value 1)
               :c (>leaf (+ ?a ?b))})

(def test-env+delay {:a (>leaf (+ 1 2))
                     :b (>leaf (p/delay 1000 (* ?a 2)))
                     :c (>leaf (p/delay 1000 (* ?a 3)))
                     :d (>leaf {:a ?a
                                :b ?b
                                :c ?c})})

(def test-env+delay-core-async {:a (>leaf (+ 1 2))
                                :b (>leaf (do (Thread/sleep 1000)
                                              (* ?a 2)))
                                :c (>leaf (do (Thread/sleep 1000)
                                              (* ?a 3)))
                                :d (>leaf {:a ?a
                                           :b ?b
                                           :c ?c})})

(def tricky-example {:x (data/value 1)
                     :y (data/value 2)
                     :a (data/value 3)
                     :b (data/value 4)
                     :c (data/value 5)
                     :w (data/branch (data/branch
                                      (>leaf (even? ?x))
                                      (>leaf ?y)
                                      (>leaf ?a))
                                     (>leaf ?b)
                                     (>leaf ?c))
                     :z (>leaf ?w)})

(def env-with-blocking-tag {:a (>leaf (Thread/currentThread))
                            :b (syntax/blocking (>leaf (Thread/currentThread)))
                            :a-name (>leaf (.getName ?a))
                            :b-name (>leaf (.getName ?b))
                            :c (>leaf (str ?a-name " " ?b-name))})

(def env-with-nine-sleeps {:a (syntax/blocking (>leaf (do (Thread/sleep 1000) :a)))
                           :b (syntax/blocking (>leaf (do (Thread/sleep 1000) :b)))
                           :c (syntax/blocking (>leaf (do (Thread/sleep 1000) :c)))
                           :d (syntax/blocking (>leaf (do (Thread/sleep 1000) :d)))
                           :e (syntax/blocking (>leaf (do (Thread/sleep 1000) :e)))
                           :f (syntax/blocking (>leaf (do (Thread/sleep 1000) :f)))
                           :g (syntax/blocking (>leaf (do (Thread/sleep 1000) :g)))
                           :h (syntax/blocking (>leaf (do (Thread/sleep 1000) :h)))
                           :i (syntax/blocking (>leaf (do (Thread/sleep 1000) :i)))
                           :z (>leaf (into #{} [?a ?b ?c ?d ?e ?f ?g ?h ?i]))})

(def env-with-sequence {:a (>leaf [1 2 3])
                        :b (syntax/>sequence inc ?a)})

(def env-with-closure-sequence {:x (data/value [1 2 3])
                                :z (data/value 2)
                                :y (>sequence #(* % ?z) ?x)})

(def env+sequence-with-nil-values
  {:a (>leaf [1 2 nil 4])
   :b (syntax/>sequence #(when % (inc %)) ?a)})

(def env+sequence-returning-nil-values
  {:a (>leaf [1 2 3 4])
   :b (syntax/>sequence #(if (even? %)
                           nil
                           %) ?a)})

(def env-with-sequence+delay {:a (>leaf [1 2 3])
                              :b (syntax/>sequence #(p/delay 1000 (inc %)) ?a)
                              :c (>leaf (mapv deref ?b))})

(def env-with-sequence+delay-sync {:a (>leaf [1 2 3])
                                   :b (syntax/>sequence #(do (Thread/sleep 1000) (inc %)) ?a)
                                   :c (>leaf ?b)})

(def env+go-block {:a (>value 1)
                   :b (>leaf (async/go (+ ?a 5))) ;;(nodely.async/>channel-leaf (async/go (+ ?a 5)))
                   :c (>leaf (+ ?a ?b))})

(def env+channel-leaf {:a (>value 1)
                       :b (nodely.async/>channel-leaf
                           (async/go (+ ?a 5)))
                       :c (>leaf (+ ?a ?b))})

(def interesting-example {:x (data/value 22)
                          :y (data/value 13)
                          :w (data/value 1)
                          :z (data/branch (>leaf (even? ?w))
                                          (>leaf (inc ?x))
                                          (>leaf (inc ?y)))})

(def env+exception {:a (>value 1)
                    :b (>leaf (throw (ex-info "Oops" {:some-data "data"})))
                    :c (>leaf (+ ?a ?b))})

(def env+channel-throw {:a (>value 1)
                        :b (nodely.async/>channel-leaf
                            (async/go (throw (ex-info "Exception in Channel"
                                                      {:data "data"}))))
                        :c (>leaf (+ ?a ?b))})

(deftest eval-key-test
  (testing "eval promise"
    (is (match? 3 (applicative/eval-key test-env :c {::applicative/context promesa/context}))))
  (testing "async works"
    (let [[time-ns result] (criterium/time-body (applicative/eval-key test-env+delay :d {::applicative/context promesa/context}))]
      (is (match? {:a 3 :b 6 :c 9} result))
      (is (match? (matchers/within-delta 100000000 1000000000) time-ns))))
  (testing "tricky example"
    (is (match? 4 (applicative/eval-key tricky-example :z {::applicative/context promesa/context})))))

(deftest eval-key-test-core-async
  (testing "eval promise"
    (is (match? 3 (applicative/eval-key test-env :c {::applicative/context core-async/context}))))
  (testing "async works"
    (let [[time-ns result] (criterium/time-body (applicative/eval-key test-env+delay-core-async
                                                                      :d
                                                                      {::applicative/context core-async/context}))]
      (is (match? {:a 3 :b 6 :c 9} result))
      (is (match? (matchers/within-delta 100000000 1000000000) time-ns))))
  (testing "tricky example"
    (is (match? 4 (applicative/eval-key tricky-example :z
                                        {::applicative/context core-async/context})))))

(deftest eval-test
  (testing "eval promise"
    (is (match? {:a {::data/value 2}
                 :b {::data/value 1}
                 :c {::data/value 3}}
                (applicative/eval test-env :c {::applicative/context promesa/context}))))
  (testing "tricky example"
    (is (match? {:x (data/value 1)
                 :y (data/value 2)
                 :a (data/value 3)
                 :b (data/value 4)
                 :c (data/value 5)
                 :w (data/value 4)
                 :z {::data/type :leaf
                     ::data/inputs #{:w}}}
                (applicative/eval tricky-example :w {::applicative/context promesa/context})))))

(deftest eval-env-with-sequence
  (testing "async response is equal to sync response"
    (is (match? (-> (core/resolve :b env-with-sequence) (get :b) ::data/value)
                (applicative/eval-key env-with-sequence :b {::applicative/context promesa/context}))))
  (testing "sync=async for sequence with nil values"
    (is (match? (-> (core/resolve :b env+sequence-with-nil-values) (get :b) ::data/value)
                (applicative/eval-key env+sequence-with-nil-values :b {::applicative/context promesa/context}))))
  (testing "sync=async for sequence returning nil values"
    (is (match? (-> (core/resolve :b env+sequence-returning-nil-values) (get :b) ::data/value)
                (applicative/eval-key env+sequence-returning-nil-values :b {::applicative/context promesa/context}))))
  (testing "async version takes a third of the time of sync version
            (runtime diff is 2 sec, within a tolerance of 3ms"
    (let [[nanosec-sync _]  (criterium/time-body (core/resolve :c env-with-sequence+delay-sync))
          [nanosec-async _] (criterium/time-body (applicative/eval-key env-with-sequence+delay :c {::applicative/context promesa/context}))]
      (is (match? (matchers/within-delta 8000000 2000000000)
                  (- nanosec-sync nanosec-async)))))
  (testing "Actually computes the correct answers"
    (is (match? [2 3 4] (applicative/eval-key env-with-sequence+delay :c {::applicative/context promesa/context}))))
  (testing "resolve closure sequence"
    (is (= [2 4 6]
           (applicative/eval-key env-with-closure-sequence :y {::applicative/context promesa/context})))))

(deftest schema-test
  (let [env-with-schema         {:a (>value 2)
                                 :b (>value 1)
                                 :c (yielding-schema (>leaf (+ ?a ?b)) s/Int)}
        env-with-failing-schema {:a (>value 2)
                                 :b (>value 1)
                                 :c (yielding-schema (>leaf (+ ?a ?b)) s/Bool)}]
    (testing "it should not fail"
      (is (match? 3 (applicative/eval-key env-with-schema :c {::applicative/context promesa/context
                                                              ::applicative/fvalidate schema/fvalidate}))))
    (testing "returns ex-info when schema is selected as fvalidate, and schema fn validation is enabled"
      (is (thrown-match? clojure.lang.ExceptionInfo
                         {:type   :schema.core/error
                          :schema java.lang.Boolean
                          :value  3}
                         (ex-data
                          (s/with-fn-validation
                            (applicative/eval-key env-with-failing-schema :c {::applicative/context promesa/context
                                                                              ::applicative/fvalidate schema/fvalidate}))))))
    (testing "doesn't validate when validation is disabled"
      (is (match? 3 (applicative/eval-key env-with-failing-schema :c {::applicative/context promesa/context
                                                                      ::applicative/fvalidate schema/fvalidate}))))))

(deftest synchronous-applicative-test
  (let [simple-env {:a (>value 2)
                    :b (>value 1)
                    :c (>leaf (+ ?a ?b))}
        env-with-failing-schema {:a (>value 2)
                                 :b (>value 1)
                                 :c (yielding-schema (>leaf (+ ?a ?b)) s/Bool)}]
    (testing "it should not fail"
      (is (match? 3 (applicative/eval-key simple-env :c {::applicative/context synchronous/context}))))
    (testing "more complicated example"
      (is (match? 4 (applicative/eval-key tricky-example :z {::applicative/context synchronous/context}))))
    (testing "returns ex-info when schema is selected as fvalidate, and schema fn validation is enabled"
      (is (thrown-match? clojure.lang.ExceptionInfo
                         {:type   :schema.core/error
                          :schema java.lang.Boolean
                          :value  3}
                         (ex-data
                          (s/with-fn-validation
                            (applicative/eval-key env-with-failing-schema :c {::applicative/fvalidate schema/fvalidate
                                                                              ::applicative/context synchronous/context}))))))))

(deftest core-async-applicative-test
  (let [simple-env {:a (>value 2)
                    :b (>value 1)
                    :c (>leaf (+ ?a ?b))}
        env-with-failing-schema {:a (>value 2)
                                 :b (>value 1)
                                 :c (yielding-schema (>leaf (+ ?a ?b)) s/Bool)}]
    (testing "it should not fail"
      (is (match? 3 (applicative/eval-key simple-env :c {::applicative/context core-async/context}))))
    (testing "more complicated example"
      (is (match? 4 (applicative/eval-key tricky-example :z {::applicative/context core-async/context}))))
    (testing "returns ex-info when schema is selected as fvalidate, and schema fn validation is enabled"
      (is (thrown-match? clojure.lang.ExceptionInfo
                         {:type   :schema.core/error
                          :schema java.lang.Boolean
                          :value  3}
                         (ex-data
                          (s/with-fn-validation
                            (applicative/eval-key env-with-failing-schema :c
                                                  {::applicative/fvalidate schema/fvalidate
                                                   ::applicative/context core-async/context}))))))
    (testing "async response is equal to sync response with async user channels"
      (is (= 7 (applicative/eval-key env+go-block :c {::applicative/context core-async/context}))))
    (testing "channel-leaf"
      (is (= 7 (applicative/eval-key env+channel-leaf :c {::applicative/context core-async/context}))))))

(deftest core-async-blocking-eval-test
  (testing "eval of a blocking tagged node will happen in the `async-thread-macro` worker pool"
    (is (match? #"async-thread-macro-\d+"
                (applicative/eval-key env-with-blocking-tag :b-name {::applicative/context core-async/context})))
    (is (match? #"async-dispatch-\d+"
                (applicative/eval-key env-with-blocking-tag :a-name {::applicative/context core-async/context})))))

(deftest thread-sleeping-test-proves-thread-works-how-we-expect
  (testing "actually only 8 threads in the async dispatch worker pool"
    (is (match? 8
                @@#'clojure.core.async.impl.exec.threadpool/pool-size)))
  (testing "when we have 9 1-second blocking nodes in one environment, it can run in fewer than 2 seconds"
    (testing "async version runs parallel when option is neglected"
      (let [[nanosec-async _] (criterium/time-body (applicative/eval-key env-with-nine-sleeps :z {::applicative/context core-async/context}))]
        (is (match? (matchers/within-delta 1000000000 1000000000)
                    nanosec-async))))))

(defspec does-not-blow-up-spec
  (prop/for-all [env (fixtures/env-gen {})]
                (applicative/eval-key env
                                      (rand-nth (keys env))
                                      {::applicative/context core-async/context})
                true))

(deftest eval-key-contextual-test
  (testing "eval node async for interesting example"
    (is (match? 14
                (async/<!! (applicative/eval-key-contextual interesting-example :z {::applicative/context core-async/context})))))
  (testing "trivial example"
    (is (match? false
                (async/<!! (applicative/eval-key-contextual {:a (data/value false)} :a {::applicative/context core-async/context})))))
  (testing "tricky example"
    (is (match? 4
                (async/<!! (applicative/eval-key-contextual tricky-example :z {::applicative/context core-async/context})))))
  (testing "env with exception"
    (is (match? {:some-data "data"}
                (ex-data (async/<!! (applicative/eval-key-contextual env+exception :c {::applicative/context core-async/context}))))))
  (testing "env with channel leaf throwing"
    (is (match?
         {:channel any?}
         (ex-data (async/<!! (applicative/eval-key-contextual env+channel-throw :c {::applicative/context core-async/context}))))))
  (testing "returns CompletableFuture when context is promesa"
    (is (match?
         java.util.concurrent.CompletableFuture
         (type (applicative/eval-key-contextual env+exception :c {::applicative/context promesa/context}))))))

(deftest eval-key-contextual-return-type-test
  (are [context expected-type]
       (= expected-type
          (type (applicative/eval-key-contextual {:a (data/value 1)} :a {::applicative/context context})))
    promesa/context     java.util.concurrent.CompletableFuture
    core-async/context  clojure.core.async.impl.channels.ManyToManyChannel
    synchronous/context nodely.engine.applicative.synchronous.Box))
(deftest manifold-applicative-test
  (let [simple-env {:a (>value 2)
                    :b (>value 1)
                    :c (>leaf (+ ?a ?b))}
        env-with-failing-schema {:a (>value 2)
                                 :b (>value 1)
                                 :c (yielding-schema (>leaf (+ ?a ?b)) s/Bool)}]
    (testing "it should not fail"
      (is (match? 3 (applicative/eval-key simple-env :c {::applicative/context manifold/context}))))

    (testing "more complicated example"
      (is (match? 4 (applicative/eval-key tricky-example :z {::applicative/context manifold/context}))))

    (testing "returns ex-info when schema is selected as fvalidate, and schema fn validation is enabled"
      (is (thrown-match? clojure.lang.ExceptionInfo
                         {:type   :schema.core/error
                          :schema java.lang.Boolean
                          :value  3}
                         (ex-data
                          (s/with-fn-validation
                            (applicative/eval-key env-with-failing-schema :c {::applicative/fvalidate schema/fvalidate
                                                                              ::applicative/context   manifold/context}))))))))

(deftest manifold-eval-key-test
  (testing "eval promise"
    (is (match? 3 (applicative/eval-key test-env :c {::applicative/context core-async/context}))))
  (testing "async works"
    (let [[time-ns result] (criterium/time-body (applicative/eval-key test-env+delay-core-async
                                                                      :d
                                                                      {::applicative/context manifold/context}))]
      (is (match? {:a 3 :b 6 :c 9} result))
      (is (match? (matchers/within-delta 100000000 1000000000) time-ns))))
  (testing "tricky example"
    (is (match? 4 (applicative/eval-key tricky-example :z
                                        {::applicative/context manifold/context})))))

(deftest manifold-eval-test
  (testing "eval promise"
    (is (match? {:a {::data/value 2}
                 :b {::data/value 1}
                 :c {::data/value 3}}
                (applicative/eval test-env :c {::applicative/context manifold/context}))))
  (testing "tricky example"
    (is (match? {:x (data/value 1)
                 :y (data/value 2)
                 :a (data/value 3)
                 :b (data/value 4)
                 :c (data/value 5)
                 :w (data/value 4)
                 :z {::data/type :leaf
                     ::data/inputs #{:w}}}
                (applicative/eval tricky-example :w {::applicative/context manifold/context})))))

(deftest manifold-eval-env-with-sequence
  (testing "async response is equal to sync response"
    (is (match? (-> (core/resolve :b env-with-sequence) (get :b) ::data/value)
                (applicative/eval-key env-with-sequence :b {::applicative/context manifold/context}))))
  (testing "sync=async for sequence with nil values"
    (is (match? (-> (core/resolve :b env+sequence-with-nil-values) (get :b) ::data/value)
                (applicative/eval-key env+sequence-with-nil-values :b {::applicative/context manifold/context}))))
  (testing "sync=async for sequence returning nil values"
    (is (match? (-> (core/resolve :b env+sequence-returning-nil-values) (get :b) ::data/value)
                (applicative/eval-key env+sequence-returning-nil-values :b {::applicative/context manifold/context}))))
  (testing "async version takes a third of the time of sync version
            (runtime diff is 2 sec, within a tolerance of 3ms"
    (let [[nanosec-sync _]  (criterium/time-body (core/resolve :c env-with-sequence+delay-sync))
          [nanosec-async _] (criterium/time-body (applicative/eval-key env-with-sequence+delay-sync :c {::applicative/context manifold/context}))]
      (is (match? (matchers/within-delta 8000000 2000000000)
                  (- nanosec-sync nanosec-async)))))
  (testing "Actually computes the correct answers"
    (is (match? [2 3 4] (applicative/eval-key env-with-sequence+delay-sync :c {::applicative/context manifold/context})))))
