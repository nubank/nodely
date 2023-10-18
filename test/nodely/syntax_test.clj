(ns nodely.syntax-test
  (:refer-clojure :exclude [cond])
  (:require
   [clojure.test :refer :all]
   [matcher-combinators.test :refer [match?]]
   [nodely.data :as data]
   [nodely.engine.core :as core]
   [nodely.syntax :as syntax :refer [>cond >if >leaf >sequence >value blocking]]))

(def data-node (data/value 42))

(deftest cond
  (testing "cond macro without leafs"
    (is (match?
         #::data {:type      :branch
                  :condition #::data {:type   :leaf
                                      :inputs #{:z}
                                      :fn     fn?}
                  :truthy    #::data {:type   :leaf
                                      :inputs #{}
                                      :fn     fn?}
                  :falsey    #::data {:type      :branch
                                      :condition #::data {:type   :leaf
                                                          :inputs #{:y}
                                                          :fn     fn?}
                                      :truthy    #::data {:type   :leaf
                                                          :inputs #{}
                                                          :fn     fn?}
                                      :falsey    #::data {:type :value :value nil}}}
         (>cond
          (>leaf (even? ?z)) (>leaf (+ 2 2))
          (>leaf (even? ?y)) (>leaf 3)))))
  (testing "cond macro with leafs"
    (is (match?
         #::data {:type      :branch
                  :condition #::data {:type   :leaf
                                      :inputs #{:z}
                                      :fn     fn?}
                  :truthy    #::data {:type   :leaf
                                      :inputs #{}
                                      :fn     fn?}
                  :falsey    #::data {:type      :branch
                                      :condition #::data {:type   :leaf
                                                          :inputs #{:y}
                                                          :fn     fn?}
                                      :truthy    #::data {:type   :leaf
                                                          :inputs #{:y :z}
                                                          :fn     fn?}
                                      :falsey    #::data {:type      :branch
                                                          :condition #::data {:type   :leaf
                                                                              :inputs #{:z}
                                                                              :fn     fn?}
                                                          :truthy    #::data {:type   :leaf
                                                                              :inputs #{}
                                                                              :fn     fn?}
                                                          :falsey    #::data {:type :value :value nil}}}}
         (>cond
          (>leaf (even? ?z)) (>leaf (+ 2 2))
          (>leaf (even? ?y)) (>leaf (- ?y (* ?z 2)))
          (>leaf (odd? ?z)) (>leaf :z-is-odd)))))
  (testing "do not resolve y if x is even"
    (is (match?
         {:x #::data {:type :value :value 2}
          :y {::data/type :leaf}
          :z #::data {:type :value :value 2}}
         (core/resolve :z {:x (data/value 2)
                           :y (data/leaf [:x] (fn [{:keys [x]}] (* x 2)))
                           :z (>cond
                               (>leaf (even? ?x)) (>leaf ?x)
                               (>leaf (even? ?y)) (>leaf ?y))}))))
  (testing "if x is not 1, y is 2*x=4"
    (is (match?
         {:x (data/value 2)
          :y (data/value 4)}
         (core/resolve :y {:x (data/value 2)
                           :y (>cond
                               (>leaf (= ?x 1)) (>leaf (+ ?x 1))
                               (>leaf true) (>leaf (* ?x 2)))}))))
  (testing "works with pure values"
    (is (match?
         {:a (data/value :go-here)}
         (core/resolve :a {:a (>cond (even? 3) :dont-go-here
                                     :else :go-here)})))))

(deftest >if-test
  (testing "if x is not 1, y is 2*2=4"
    (is (match? {:x (data/value 2)
                 :y (data/value 4)}
                (core/resolve :y {:x (data/value 2)
                                  :y (>if (>leaf (= ?x 1))
                                          (>leaf (+ ?x 1))
                                          (>leaf (* ?x 2)))}))))
  (testing "works with pure values"
    (is (match? {:a (data/value :go-here)}
                (core/resolve :a {:a (>if (even? 3) :dont-go-here :go-here)})))))

(deftest direct-node-definition
  (testing "we can support nested cond"
    (is (match? #:nodely.data{:type :branch,
                              :condition
                              #:nodely.data{:type :leaf,
                                            :inputs  #{:z},
                                            :fn   fn?},
                              :truthy
                              #:nodely.data{:type  :value,
                                            :value 4},
                              :falsey
                              #:nodely.data{:type   :branch,
                                            :condition
                                            #:nodely.data{:type :leaf,
                                                          :inputs  #{:y},
                                                          :fn   fn?},
                                            :truthy
                                            #:nodely.data{:type :branch,
                                                          :condition
                                                          #:nodely.data{:type :leaf,
                                                                        :inputs  #{:y},
                                                                        :fn   fn?},
                                                          :truthy
                                                          #:nodely.data{:type :leaf,
                                                                        :inputs  #{:y},
                                                                        :fn   fn?},
                                                          :falsey
                                                          #:nodely.data{:type :branch,
                                                                        :condition
                                                                        #:nodely.data{:type
                                                                                      :leaf,
                                                                                      :inputs #{:z},
                                                                                      :fn fn?},
                                                                        :truthy
                                                                        #:nodely.data{:type :value
                                                                                      :value :z-is-odd},
                                                                        :falsey
                                                                        #:nodely.data{:type
                                                                                      :value,
                                                                                      :value
                                                                                      nil}}},
                                            :falsey #:nodely.data{:type :value, :value nil}}}
                (>cond
                 (>leaf (even? ?z)) (+ 2 2)
                 (>leaf (even? ?y)) (>cond
                                     (>leaf (= 42 ?y)) (>leaf (- 42 ?y))
                                     (>leaf (odd? ?z)) :z-is-odd)))))
  (testing "inside env"
    (is (match?
         {:a #::data {:type  :value
                      :value 42}}
         {:a (data/value 42)})))
  (testing "variable node reference"
    (is (match?
         {:a #::data {:type  :value
                      :value 42}}
         {:a data-node})))
  (testing "local binding node reference"
    (let [data-node (data/value 42)]
      (is (match?
           {:a #::data {:type  :value
                        :value 42}}
           {:a data-node}))))
  (testing "sequence"
    (is (match?
         {:a #::data {:type   :leaf
                      :inputs #{}
                      :fn     fn?}
          :b #::data {:type  :sequence
                      :input :a
                      :fn-fn fn?}}
         {:a (>leaf [1 2 3])
          :b (>sequence inc ?a)}))))

(deftest >or-test
  (testing "returns first truthy value"
    (is (match? {:a (data/value 4)}
                (core/resolve :a {:a (syntax/>or (data/value nil) (data/value nil) (data/value 4))}))))
  (testing "infers values"
    (is (match? {:a (data/value 4)}
                (core/resolve :a {:a (syntax/>or nil nil 4)}))))
  (testing "or last falsey value"
    (is (match? {:a (data/value false)}
                (core/resolve :a {:a (syntax/>or (data/value nil) (data/value nil) (data/value false))}))))
  (testing "infers value"
    (is (match? {:a (data/value 4)}
                (core/resolve :a {:a (syntax/>or nil false 4)})))))

(deftest >and-test
  (testing "only one node"
    (is (match? {:a (data/value 1)}
                (core/resolve :a {:a (syntax/>and 1)}))))
  (testing "returns first falsey value"
    (is (match? {:a (data/value false)}
                (core/resolve :a {:a (syntax/>and (data/value 1) (data/value false) (data/value 2))}))))
  (testing "or last truthy value"
    (is (match? {:a (data/value 3)}
                (core/resolve :a {:a (syntax/>and (data/value 1) (data/value 2) (data/value 3))})))))

(deftest leaf
  (testing "should build node of leaf"
    (is (match? #::data{:type :leaf :inputs #{:x :y} :fn ifn?}
                (>leaf (+ ?x ?y))))))

(deftest value
  (testing "builds a value"
    (is (match? #::data{:type :value :value 1}
                (>value 1)))))

(deftest blocking-builds-blocking-nodes
  (testing "Flags a node with the blocking tag"
    (is (match? #::data{:type :leaf :inputs #{:x :y} :fn ifn? :tags #{::data/blocking}}
                (blocking (>leaf (+ ?x ?y)))))))
