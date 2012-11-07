{:user {:plugins [[lein-vanity "0.1.0"]
                  [jonase/eastwood "0.0.2"]
                  [lein-swank "1.4.4"]]
        :dependencies {clj-stacktrace "0.2.5"}
        :injections [(let [orig (ns-resolve (doto 'clojure.stacktrace require)
                                            'print-cause-trace)
                           new (ns-resolve (doto 'clj-stacktrace.repl require)
                                           'pst)]
                       (alter-var-root orig (constantly @new)))]}}

