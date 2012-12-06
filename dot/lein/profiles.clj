{:user {:plugins [[lein-vanity "0.1.0"]
                  [jonase/eastwood "0.0.2"]
                  [lein-ritz "0.6.0"]]
        :dependencies [[ritz/ritz-nrepl-middleware "0.6.0"]
                       [ritz/ritz-debugger "0.6.0"]
                       [ritz/ritz-repl-utils "0.6.0"]
                       [clojure-complete "0.2.2"]]
        :repl-options {:nrepl-middleware
                       [ritz.nrepl.middleware.javadoc/wrap-javadoc
                        ritz.nrepl.middleware.simple-complete/wrap-simple-complete]}}
 :hooks [ritz.add-sources]}
