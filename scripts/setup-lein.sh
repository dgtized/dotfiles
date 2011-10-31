#!/bin/bash

rm -rf ~/.lein
lein self-install
lein plugin install swank-clojure 1.3.3
