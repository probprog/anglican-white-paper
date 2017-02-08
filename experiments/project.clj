(defproject anglican-user "0.1.0-SNAPSHOT"
  :description "Performance comparison"
  :url "https://bitbucket.org/probprog/anglican-white-paper"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [nstools "0.2.4"]
                 [anglican "1.0.0"]]
  :plugins [[dtolpin/lein-gorilla "0.3.7-SNAPSHOT"]]
  :resource-paths ["programs"]
  :main ^:skip-aot anglican.core)
