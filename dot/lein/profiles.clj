{:user
 {:plugins [[lein-vanity "0.2.0" :exclusions [org.clojure/clojure]]
            [lein-ancient "0.6.5"]
            [jonase/eastwood "0.2.0"]
            [refactor-nrepl "2.2.0-SNAPSHOT"]]
  :dependencies [[slamhound "1.5.5"]
                 [org.clojure/tools.nrepl "0.2.12"]]
  :aliases {"slamhound" ["run" "-m" "slam.hound"]}}}
