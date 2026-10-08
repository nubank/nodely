(ns nodely.engine.protocols
  (:refer-clojure :exclude [eval]))

(defprotocol Engine
  (-eval [engine env k opts] "Resolves node `k` in `env` using `engine`, returning the resulting env with `k` and its dependencies filled in with their evaluated values.")
  (-eval-key [engine env k opts] "Resolves node `k` in `env` using `engine`, returning just the evaluated value of `k`.")
  (-eval-key-channel [engine env k opts] "Resolves node `k` in `env` using `engine`, returning a core.async channel that yields the evaluated value of `k`.")
  (-eval-key-channel-supported? [engine] "Returns true if `engine` supports `-eval-key-channel`, false otherwise.")
  (-enable-deref [engine] "Returns a delay that yields nil if `engine` is available for use, or a map describing why it could not be enabled (e.g. a missing optional dependency) otherwise.")
  (-prepare-opts [engine opts] "Transforms the caller-supplied `opts` map into the opts map expected by `engine`'s")
  (-pre-eval-hook [engine hook] "Set-up a function that will run **before** every single node is evaluated, returning an engine with the pre-eval hook installed")
  (-post-eval-hook [engine hook] "Set-up a function that will run **after** every single node is evaluated, returning an engine with the post-eval hook installed"))

(comment
  ;; => if I run pre-eval hook on an engine twice are both hooks installed? or does the second hook overwrite the first hook?
  ;;  => If both hooks install what the order they get executed??

  ;; => can I install a hook for only specific nodes? How can I match? Is it by node name or by node type?

  ;; => when the hook gets called what information does it get?
  ;;  => for profiling the hook has to receive the node name so it can record the time it was ran

  ;; => are the hooks reentrant?
  )


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

(defn eval-key-channel-supported?
  [engine]
  (-eval-key-channel-supported? engine))
