(ns nodely.engine.protocols)

(defprotocol Engine
  (-eval [engine env k opts] "Applies function f to the value(s) inside the context of the functor fv.")
  (-eval-key [engine env k opts] "Applies function f to the value(s) inside the context of the functor fv.")
  (-eval-key-channel [engine env k opts] "Applies function f to the value(s) inside the context of the functor fv.")
  (-eval-key-channel-supported? [engine] "Applies function f to the value(s) inside the context of the functor fv.")
  (-enable-deref [engine] "Please claude update the docstring"))

(defn eval
  ([engine env k]
   (-eval engine env k {}))
  ([engine env k opts]
   (-eval engine env k opts)))

(defn eval-key
  ([engine env k]
   (-eval-key engine env k {}))
  ([engine env k opts]
   (-eval-key engine env k opts)))

(defn eval-key-channel
  ([engine env k]
   (-eval-key-channel engine env k {}))
  ([engine env k opts]
   (-eval-key-channel engine env k opts)))
