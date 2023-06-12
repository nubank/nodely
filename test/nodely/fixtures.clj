(ns nodely.fixtures
  (:require [clojure.test.check.generators :as gen]
            [nodely.data :as data]))

(defn branch-count
  [env]
  (count (filter #(= (::data/type %) :branch) (mapcat data/flatten (vals env)))))

(defn subset
  [s]
  (gen/let [n (gen/choose 0 (count s))
            ss (gen/shuffle s)]
    (set (take n ss))))

(def value-gen
  (gen/let [v (gen/one-of [gen/boolean (gen/return nil)])]
    (data/value v)))

(defn leaf-gen
  [env]
  (gen/let [inputs (subset (keys env))]
    (data/leaf inputs identity)))

(defn scalar-gen
  [env]
  (gen/one-of [value-gen (leaf-gen env)]))

(defn branch-gen
  [env g]
  (gen/let [n1 (gen/one-of [(scalar-gen env) g])
            n2 (gen/one-of [(scalar-gen env) g])
            n3 (gen/one-of [(scalar-gen env) g])]
    (data/branch n1 n2 n3)))

(defn node-gen
  [env]
  (gen/recursive-gen (partial branch-gen env)
                     (scalar-gen env)))

(defn sequence-gen
  "Generates two nodes:
  One leaf node made up of combining random keys from env into a vector
  A sequence node made up applying identity to this input
  Returns an env with those two nodes only"
  [env]
  (gen/let [inputs (subset (keys env))
            input-node (gen/return (data/leaf inputs (fn [env] (vec (vals (select-keys env inputs))))))
            [input-key sequence-key] (gen/fmap seq (gen/set gen/keyword {:num-elements 2}))]
    {input-key    input-node
     sequence-key (data/sequence input-key identity)}))

(defn env-stage-gen
  [{:keys [add-sequence? env max-nodes min-nodes node-generator]
    :or   {min-nodes      1
           max-nodes      4
           env            {}
           add-sequence?  true
           node-generator (fn [_] value-gen)}}]
  (gen/let [number-of-nodes (gen/choose min-nodes max-nodes)
            nodes           (gen/vector (node-generator env) number-of-nodes)
            keys            (gen/vector gen/keyword number-of-nodes)
            sequence-env    (if add-sequence? (sequence-gen env) (gen/return {}))]
    (into sequence-env (for [k keys
                             v nodes]
                         [k v]))))

(defn env-stages-gen
  [{:keys [max-nodes-per-stage
           min-nodes-per-stage
           node-generator
           number-of-stages]
    :or   {max-nodes-per-stage 4
           min-nodes-per-stage 1
           node-generator      node-gen
           number-of-stages    3}
    :as   options}]
  (cond
    (= 1 number-of-stages)
    (env-stage-gen {:max-nodes     max-nodes-per-stage
                    :min-nodes     min-nodes-per-stage
                    :add-sequence? false})
    :else
    (let [prev-stages-gen (env-stages-gen (update options :number-of-stages dec))]
      (gen/bind prev-stages-gen
                (fn [m]
                  (let [inputs (keys m)]
                    (gen/let [next (env-stage-gen {:min-nodes      min-nodes-per-stage
                                                   :max-nodes      max-nodes-per-stage
                                                   :env            m
                                                   :node-generator node-generator})]
                      (merge next m))))))))

(defn env-gen
  [{:keys [max-nodes-per-stage
           min-nodes-per-stage
           min-stages
           max-stages
           node-generator
           number-of-stages
           max-branch-count]
    :or   {max-nodes-per-stage 4
           min-nodes-per-stage 1
           min-stages          1
           max-stages          3
           node-generator      node-gen
           number-of-stages    3}
    :as   options}]
  (let [generator (gen/let [number-of-stages (gen/choose min-stages max-stages)
                            env (env-stages-gen (assoc options :number-of-stages number-of-stages))]
                    env)]
    (if max-branch-count
      (gen/such-that #(< (branch-count %) max-branch-count) generator)
      generator)))
