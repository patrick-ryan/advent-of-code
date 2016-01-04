(defproject advent-of-code-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
  				 [org.clojure/clojure-contrib "1.2.0"]
  				 [digest "1.4.4"]]
  :main ^:skip-aot advent-of-code-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
