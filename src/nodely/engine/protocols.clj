(ns nodely.engine.protocols)

(defprotocol Engine
  (-eval [engine env k opts] "Resolves node `k` in `env` using `engine`, returning the resulting env with `k` and its dependencies filled in with their evaluated values.")
  (-eval-key [engine env k opts] "Resolves node `k` in `env` using `engine`, returning just the evaluated value of `k`.")
  (-eval-key-channel [engine env k opts] "Resolves node `k` in `env` using `engine`, returning a core.async channel that yields the evaluated value of `k`.")
  (-eval-key-channel-supported? [engine] "Returns true if `engine` supports `-eval-key-channel`, false otherwise.")
  (-enable-deref [engine] "Returns a delay that yields nil if `engine` is available for use, or a map describing why it could not be enabled (e.g. a missing optional dependency) otherwise.")
  (-prepare-opts [engine opts] "Transforms the caller-supplied `opts` map into the opts map expected by `engine`'s"))

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
