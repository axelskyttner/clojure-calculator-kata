(defproject calculator "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.8.0"]
                 ]

  :plugins [[lein-auto "0.1.3"]]
  ;:main ^:skip-aot calculator.core
  :main calculator.core
  :aot [calculator.core]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
