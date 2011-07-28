#!/bin/sh
# kick test with SpiderMonkey
js -f ../src/deps/underscore.js \
   -f ../src/deps/underscore.string.js \
   -f ../src/platforms/spidermonkey/console.js \
   -f ../lib/console_biwascheme.js \
   -f ./console_test.js
