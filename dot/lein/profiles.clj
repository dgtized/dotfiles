{:user
 {:plugins [[lein-vanity "0.2.0" :exclusions [org.clojure/clojure]]
            [lein-ancient "0.6.14"]
            [lein-kibit "0.1.6"]
            [jonase/eastwood "0.2.5"]
            ;;[com.billpiel/sayid "0.0.10"]
            ]
  :dependencies [[slamhound "1.5.5"]]
  :aliases {"slamhound" ["run" "-m" "slam.hound"]}}}
