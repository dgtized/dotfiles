{:user
 {:plugins [[lein-vanity "0.2.0" :exclusions [org.clojure/clojure]]
            [lein-ancient "0.6.15"]
            [lein-hiera "1.0.0"]
            [lein-kibit "0.1.6"]
            [lein-pprint "1.2.0"]
            [jonase/eastwood "0.3.4"]
            ;; [com.billpiel/sayid "0.0.18"]
            [lein-cljfmt "0.6.3"]]
  :dependencies [[slamhound "1.5.5"]]
  :aliases {"slamhound" ["run" "-m" "slam.hound"]}}}
