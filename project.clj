(defproject chess-gm "0.1.0-SNAPSHOT"
  :description "A reactive chess game built to be asynchronous"
  :url "https:www.github.com/djtango/chess-gm"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-beta2"]
                 [midje "1.9.0-alpha6"]]

  :source-paths ["src/clj"]
  :plugins [[lein-midje "3.1.1"]]
  :repl-options {:init-ns chess-gm.core
                 :init (do
                         (require '[midje.repl :as repl])
                         (repl/autotest))})
