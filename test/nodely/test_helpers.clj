(ns nodely.test-helpers
  (:require
   [clojure.string :as string]
   [clojure.test :as t]
   [matcher-combinators.clj-test]
   [matcher-combinators.result :as result]
   [matcher-combinators.standalone :as matcher-combinators :refer [match]]))

(defn- clojure-test-report
  [{:keys [expected actual description]
    result :match/result
    detail :mismatch/detail
    :as    report}]
  (case result
    :match
    {:type :pass}

    :mismatch
    {:type     :fail
     :message  description
     :expected expected
     :actual   (matcher-combinators.clj-test/tagged-for-pretty-printing
                (list '~'not (list 'match? expected actual))
                {::result/value detail})
     :file     (:file report)
     :line     (:line report)}))

(defmacro matching
  ([expected actual]
   (matching "Oops!" expected actual))
  ([description expected actual]
   `(assoc (matcher-combinators.clj-test/with-file+line-info
             (match ~expected ~actual))
           :expected ~expected
           :actual ~actual
           :description ~description)))

(defmacro deftest
  {:doc      "Clojure.test: The good parts"
   :arglists '([name & assertions])}
  ([name assertions]
   `(let [assertions# ~assertions]
      (t/deftest ~name
        (doseq [assertion-data# assertions#]
          (t/report (#'clojure-test-report assertion-data#))))))
  ([name assertions & more]
   `(deftest ~name (concat ~assertions ~@more))))

(defn testing
  [description & assertions]
  (map (fn [assertion] (update assertion :description (partial str description " - "))) (flatten assertions)))

(comment
  (deftest my-test
    (testing "Top layer"
      (matching 1 2)
      (matching 1 1))
    (testing "My description"
      (testing "nested description"
        (matching "A description" 1 2)
        (matching 1 1)))
    (testing "one more layer"
      (matching 1 2)
      (matching 1 1))))
