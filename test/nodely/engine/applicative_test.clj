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
   [nodely.engine.applicative.synchronous :as synchronous]
   [nodely.engine.core :as core]
   [nodely.engine.core-async.core :as nodely.async]
   [nodely.engine.schema :as schema]
   [nodely.fixtures :as fixtures]
   [nodely.syntax :as syntax :refer [>leaf >value]]
   [nodely.syntax.schema :refer [yielding-schema]]
   [promesa.core :as p]
   [schema.core :as s]
   [clojure.test.check.generators :as gen]
   [loom.alg :as alg]))

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

(def env-with-sequence {:a (>leaf [1 2 3])
                        :b (syntax/>sequence inc ?a)})

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

(deftest eval-key-test
  (testing "eval promise"
    (is (match? 3 (applicative/eval-key test-env :c))))
  (testing "async works"
    (let [[time-ns result] (criterium/time-body (applicative/eval-key test-env+delay :d))]
      (is (match? {:a 3 :b 6 :c 9} result))
      (is (match? (matchers/within-delta 100000000 1000000000) time-ns))))
  (testing "tricky example"
    (is (match? 4 (applicative/eval-key tricky-example :z)))))

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
                (applicative/eval test-env :c))))
  (testing "tricky example"
    (is (match? {:x (data/value 1)
                 :y (data/value 2)
                 :a (data/value 3)
                 :b (data/value 4)
                 :c (data/value 5)
                 :w (data/value 4)
                 :z {::data/type :leaf
                     ::data/inputs #{:w}}}
                (applicative/eval tricky-example :w)))))

(deftest eval-env-with-sequence
  (testing "async response is equal to sync response"
    (is (match? (-> (core/resolve :b env-with-sequence) (get :b) ::data/value)
                (applicative/eval-key env-with-sequence :b))))
  (testing "sync=async for sequence with nil values"
    (is (match? (-> (core/resolve :b env+sequence-with-nil-values) (get :b) ::data/value)
                (applicative/eval-key env+sequence-with-nil-values :b))))
  (testing "sync=async for sequence returning nil values"
    (is (match? (-> (core/resolve :b env+sequence-returning-nil-values) (get :b) ::data/value)
                (applicative/eval-key env+sequence-returning-nil-values :b))))
  (testing "async version takes a third of the time of sync version
            (runtime diff is 2 sec, within a tolerance of 3ms"
    (let [[nanosec-sync _]  (criterium/time-body (core/resolve :c env-with-sequence+delay-sync))
          [nanosec-async _] (criterium/time-body (applicative/eval-key env-with-sequence+delay :c))]
      (is (match? (matchers/within-delta 8000000 2000000000)
                  (- nanosec-sync nanosec-async)))))
  (testing "Actually computes the correct answers"
    (is (match? [2 3 4] (applicative/eval-key env-with-sequence+delay :c)))))

(deftest schema-test
  (let [env-with-schema         {:a (>value 2)
                                 :b (>value 1)
                                 :c (yielding-schema (>leaf (+ ?a ?b)) s/Int)}
        env-with-failing-schema {:a (>value 2)
                                 :b (>value 1)
                                 :c (yielding-schema (>leaf (+ ?a ?b)) s/Bool)}]
    (testing "it should not fail"
      (is (match? 3 (applicative/eval-key env-with-schema :c {::applicative/fvalidate schema/fvalidate}))))
    (testing "returns ex-info when schema is selected as fvalidate, and schema fn validation is enabled"
      (is (thrown-match? clojure.lang.ExceptionInfo
                         {:type   :schema.core/error
                          :schema java.lang.Boolean
                          :value  3}
                         (ex-data
                          (s/with-fn-validation
                            (applicative/eval-key env-with-failing-schema :c {::applicative/fvalidate schema/fvalidate}))))))
    (testing "doesn't validate when validation is disabled"
      (is (match? 3 (applicative/eval-key env-with-failing-schema :c {::applicative/fvalidate schema/fvalidate}))))))

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

(defspec does-not-blow-up-spec
  (prop/for-all [env (fixtures/env-gen {})]
                (applicative/eval-key env
                                      (rand-nth (keys env))
                                      {::applicative/context core-async/context})
                true))

(deftest compare-engines
  (let [sample-env (gen/generate (fixtures/env-gen {:node-generator fixtures/scalar-gen
                                                    :min-stages     10
                                                    :max-stages     10}))
        a-key      (last (alg/topsort (core/env->graph sample-env)))]
    #_(testing ""
      (is (= 234 (applicative/eval-key sample-env a-key {::applicative/context core-async/context}))))))
