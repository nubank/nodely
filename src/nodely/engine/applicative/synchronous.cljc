(ns nodely.engine.applicative.synchronous
  (:require
   [cats.protocols :as mp]
   [nodely.engine.applicative.protocols :as protocols]))

(declare context)

(defrecord Box [value]
  mp/Contextual
  (-get-context [_] context)

  mp/Extract
  (-extract [it] value))

(defn box
  [it]
  (->Box it))

(defn unbox
  [it]
  (:value it))

(def ^:no-doc context
  (reify
    mp/Context
    protocols/RunNode
    (-apply-fn [_ f mv]
      (box (f (unbox mv))))
    mp/Functor
    (-fmap [_ f mv]
      (box (f (unbox mv))))

    mp/Monad
    (-mreturn [_ v]
      (box v))

    ;; Box a -> (a -> Box b) -> Box b
    (-mbind [_ mv f]
      (f (unbox mv)))

    mp/Applicative
    (-pure [_ v]
      (box v))
    (-fapply [_ pf pv]
      (box ((unbox pf) (unbox pv))))))

(comment

  mbind powers this mlet
  mbind is invoked
  (mlet [x [1 2 3]]
        (+ 1 x)))
