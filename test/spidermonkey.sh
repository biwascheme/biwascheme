#!/bin/sh
# kick test with SpiderMonkey
js -f ../src/console/console-spidermonkey.js \
   -f ../lib/console_biwascheme.js \
   -f ./console_test.js
