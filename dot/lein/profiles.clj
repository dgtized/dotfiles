{:user
 {:plugins [[lein-vanity "0.1.0"]
            [jonase/eastwood "0.0.2"]
            [lein-ritz "0.7.0"]
            [com.palletops/pallet-lein "0.8.0-alpha.1"]]
  :dependencies [[ritz/ritz-nrepl-middleware "0.7.0"]
                 [slamhound "1.3.3"]]
  :repl-options {:nrepl-middleware
                 [ritz.nrepl.middleware.javadoc/wrap-javadoc
                  ritz.nrepl.middleware.simple-complete/wrap-simple-complete]}
  :aliases {"slamhound" ["run" "-m" "slam.hound"]}}}
