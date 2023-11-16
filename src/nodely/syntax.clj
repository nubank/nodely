(ns nodely.syntax
  (:require
   [clojure.string :as string]
   [nodely.data :as data]))

(defn- expression-symbols
  [expr]
  (set (filter (complement seqable?) (tree-seq seqable? seq expr))))

(defn- fn-with-arg-map
  [args expr]
  (let [arg-map (->> args
                     (map (fn [s]
                            [s (-> s name (subs 1) keyword)]))
                     (into {}))]
    (list `fn [(or (not-empty arg-map) '_)]
          expr)))

(defn- question-mark->keyword
  [s]
  (-> (name s) (subs 1) keyword))

(defn- question-mark-symbols
  [expr]
  (filter #(string/starts-with? (str %) "?") (expression-symbols expr)))

(defn- assert-not-shadowing!
  [symbols]
  (let [shadowed-symbols (filter #(clojure.core/resolve %) symbols)]
    (assert (empty? shadowed-symbols) (str "Identifiers with '?' cannot clash with existing bindings " shadowed-symbols))))

(defn infer-value
  [n]
  (if (data/node? n)
    n
    (data/value n)))

(defmacro >sequence
  [fn input]
  (list `data/sequence (question-mark->keyword input) fn))

(defmacro >leaf
  [expr]
  (let [symbols-to-be-replaced (question-mark-symbols expr)]
    (assert-not-shadowing! symbols-to-be-replaced)
    (list `data/leaf (mapv (comp keyword #(subs % 1) name) symbols-to-be-replaced)
          (fn-with-arg-map symbols-to-be-replaced expr))))

(defn >and
  ([] (data/value true))
  ([n] (infer-value n))
  ([n & remaining]
   (let [node (infer-value n)]
     (data/branch node (apply >and remaining) node))))

(defn >or
  ([] (data/value nil))
  ([n] (infer-value n))
  ([n & remaining]
   (let [n (infer-value n)]
     (data/branch n n (apply >or remaining)))))

(defn- >cond-aux
  ([] (data/value nil))
  ([[condition node] & remaining]
   (data/branch (infer-value condition) (infer-value node) (apply >cond-aux remaining))))

;; TODO: Break when number of clauses not even
(defn >cond
  [& clauses]
  (apply #'>cond-aux (partition-all 2 clauses)))

(defn >if
  [condition left right]
  (data/branch (infer-value condition) (infer-value left) (infer-value right)))

(defn >value
  [v]
  (data/value v))

(defn blocking
  [node]
  (update node ::data/tags conj ::data/blocking))
