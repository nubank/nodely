(ns nodely.test-helpers
  (:require  [clojure.test :as t]
             [matcher-combinators.standalone :as matcher-combinators :refer [match]]
             [matcher-combinators.result :as result]
             [matcher-combinators.clj-test]))

(defn- clojure-test-report
  [{:keys [expected actual]
    result :match/result
    detail :mismatch/detail
    :as    report}]
  (case result
    :match
    {:type :pass}

    :mismatch
    {:type     :fail
     :message  "Oops!" ;; TODO: Have better messages
     :expected expected
     :actual   (matcher-combinators.clj-test/tagged-for-pretty-printing
                (list '~'not (list 'match? expected actual))
                {::result/value detail})
     :file     (:file report)
     :line     (:line report)}))

(defmacro matching
  [expected actual]
  `(assoc (matcher-combinators.clj-test/with-file+line-info
            (match ~expected ~actual))
          :expected ~expected
          :actual ~actual))

(defmacro deftest
  {:doc      "Clojure.test: The good parts"
   :arglists '([name & assertions])}
  [name assertions]
  `(let [assertions# ~assertions]
     (t/deftest ~name
       (doseq [assertion-data# assertions#]
         (t/report (#'clojure-test-report assertion-data#))))))

(comment
  (deftest my-test
    [(matching 1 2)
     (matching 1 1)]))
