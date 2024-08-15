(ns nodely.engine.applicative.synchronous
  (:require [nodely.engine.applicative.protocols :as protocols]))

(declare context)

(defrecord Box [value]
  protocols/Contextual
  (-get-context [_] context)

  protocols/Extract
  (-extract [it] value))

(defn box
  [it]
  (->Box it))

(defn unbox
  [it]
  (:value it))

(def ^:no-doc context
  (reify
    protocols/Context
    protocols/RunNode
    (-apply-fn [_ f mv]
      (box (f (unbox mv))))
    protocols/Functor
    (-fmap [_ f mv]
      (box (f (unbox mv))))

    protocols/Monad
    (-mreturn [_ v]
      (box v))

    ;; Box a -> (a -> Box b) -> Box b
    (-mbind [_ mv f]
      (f (unbox mv)))

    protocols/Applicative
    (-pure [_ v]
      (box v))
    (-fapply [_ pf pv]
      (box ((unbox pf) (unbox pv))))))

(comment

  mbind powers this mlet
  mbind is invoked
  (mlet [x [1 2 3]]
        (+ 1 x)))
