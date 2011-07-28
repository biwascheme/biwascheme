#!/bin/sh
# kick test with SpiderMonkey
js -f ../src/underscore.js \
   -f ../src/underscore.string.js \
   -f ../src/console/spidermonkey-console.js \
   -f ../lib/console_biwascheme.js \
   -f ./console_test.js
