(defproject temperaments "0.1.0-SNAPSHOT"
  :description "Explore alternate tunings and necklace patterns"
  :jvm-opts ^:replace []
  :main ^{:skip-aot true} temperaments.core
  :url "mailto:dabrowsa@indiana.edu"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [overtone "0.10.1"]
                 [seesaw "1.4.5"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :target-path "target/%s"
)
