(defproject dev.nu/nodely "2.0.0"
  :description "Decoupling data fetching from data dependency declaration"
  :url "https://github.com/nubank/nodely"
  :license {:name "MIT"}

  :plugins [[s3-wagon-private "1.3.4" :exclusions [com.fasterxml.jackson.core/jackson-core]]
            [com.fasterxml.jackson.core/jackson-core "2.12.4"]]

  :dependencies [[org.clojure/clojure "1.10.3"]
                 [aysylu/loom "1.0.2"]
                 [org.clojure/core.async "1.5.648" :scope "provided"]
                 [funcool/promesa "10.0.594" :scope "provided"]
                 [manifold "0.1.9-alpha5" :scope "provided"]
                 [prismatic/schema "1.1.12"]]

  :exclusions [log4j]

  :repositories [["publish" {:url "https://clojars.org/repo"
                             :username :env/clojars_username
                             :password :env/clojars_passwd
                             :sign-releases false}]]

  :repl-options {:init-ns user}

  :profiles {:uberjar {:aot :all}
             :dev     {:source-paths ["dev"]
                       :plugins [[com.github.clojure-lsp/lein-clojure-lsp "0.1.1"]
                                 [com.github.clj-kondo/lein-clj-kondo "0.1.1"]]
                       :dependencies [[nubank/matcher-combinators "3.1.4"]
                                      [prismatic/schema-generators "0.1.5"]
                                      [criterium "0.4.6"]
                                      [org.clojure/tools.namespace "1.1.0"]]}}

  :aliases {"unit"         ["test"]
            "clj-kondo"    ["do"
                            ["clj-kondo" "--copy-configs" "--dependencies" "--config" ".clj-kondo/config.edn" "--lint" "$classpath"]
                            ["clj-kondo" "--config" ".clj-kondo/config.edn" "--lint" "src" "test"]]
            "format"       ["clojure-lsp" "format" "{:dry? true}"]
            "format-fix"   ["clojure-lsp" "format"]
            "clean-ns"     ["clojure-lsp" "clean-ns" "{:dry? true}"]
            "clean-ns-fix" ["clojure-lsp" "clean-ns"]
            "lint"         ["do" ["format"] ["clean-ns"]]
            "lint-fix"     ["do" ["format-fix"] ["clean-ns-fix"]]}

  :min-lein-version "2.4.2"

  :resource-paths ["resources"]
  :test-paths ["test/"])
