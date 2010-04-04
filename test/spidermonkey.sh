#!/bin/sh
# kick test with SpiderMonkey
js -f ../src/console/spidermonkey-console.js \
   -f ../lib/console_biwascheme.js \
   -f ./console_test.js
