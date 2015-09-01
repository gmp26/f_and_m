#!/bin/bash

cd ~/clojure/f_and_m
lein clean
lein cljsbuild once min
#rsync -av resources/public/* gmp26@maths.org:/www/nrich/html/factmult
