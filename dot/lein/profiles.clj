{:user
 {:plugins [[lein-vanity "0.1.0"]
            [jonase/eastwood "0.0.2"]
            [lein-ritz "0.7.0"]
            [com.palletops/pallet-lein "0.6.0-beta.7"]]
  :dependencies [[ritz/ritz-nrepl-middleware "0.7.0"]
                 [clojure-complete "0.2.2"]
                 [slamhound "1.3.3"]
                 [javert "0.2.0-SNAPSHOT"]]
  :repl-options {:nrepl-middleware
                 [inspector.middleware/wrap-inspect
                  ritz.nrepl.middleware.javadoc/wrap-javadoc
                  ritz.nrepl.middleware.apropos/wrap-apropos]}
  :aliases {"slamhound" ["run" "-m" "slam.hound"]}}}
