(defproject kawoosh "1.0.0-SNAPSHOT"
  :description "REST to IRC gateway"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [noir "1.2.1"]
                 [postgresql "9.1-901.jdbc4"]
                 [clojureql "1.0.4"]
                 [org.clojure/java.jdbc "0.2.3"]]
  :main kawoosh.server)