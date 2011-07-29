#
# Makefile - gather javascripts and compress it
#
VERSION_FILE_IN =  src/version.js.in
VERSION_FILE    =  src/version.js

BASIC_FILES =                                     \
  src/system/class.js                             \
  src/stackbase.js                                \
  src/system/set.js                               \
  src/system/write.js                             \
  src/system/pair.js                              \
  src/system/value.js                             \
  src/system/symbol.js                            \
  src/system/char.js                              \
  src/system/number.js                            \
  src/system/port.js                              \
  src/system/record.js                            \
  src/system/hashtable.js                         \
  src/system/syntax.js                            \
  src/system/types.js                             \
  src/system/parser.js                            \
  src/system/compiler.js                          \
  src/system/pause.js                             \
  src/system/call.js                              \
  src/system/interpreter.js                       \
  src/library/infra.js                            \
  src/library/r6rs_lib.js                         \
  src/library/js_interface.js                     \
  src/library/extra_lib.js                        \
  src/library/srfi.js

BROWSER_FILES =                                   \
  src/deps/jquery.js                              \
  src/deps/underscore.js                          \
  src/deps/underscore.string.js                   \
  $(BASIC_FILES)                                  \
  src/library/webscheme_lib.js                    \
  src/platforms/browser/dumper.js                 \
  src/platforms/browser/console.js                \
  src/platforms/browser/release_initializer.js

CONSOLE_FILES =                                   \
  $(BASIC_FILES)

all: build

build: lib/biwascheme.js lib/biwascheme-min.js lib/console_biwascheme.js node_modules/biwascheme/lib/biwascheme.js

$(VERSION_FILE): $(VERSION_FILE_IN) $(BROWSER_FILES) $(CONSOLE_FILES) VERSION Makefile
	cat $< | sed -e "s/@GIT_COMMIT@/`git log -1 --pretty=format:%H`/" | sed -e "s/@VERSION@/`cat VERSION`/" > $@

lib/biwascheme.js: $(VERSION_FILE) $(BROWSER_FILES) Makefile
	cat $(VERSION_FILE) > $@
	cat $(BROWSER_FILES) >> $@
	@echo "Wrote " $@

lib/biwascheme-min.js: lib/biwascheme.js
	java -jar bin/yuicompressor-2.4.2.jar lib/biwascheme.js -o $@
	@echo "Wrote " $@

lib/console_biwascheme.js: $(VERSION_FILE) $(CONSOLE_FILES) Makefile
	cat $(VERSION_FILE) > $@
	cat $(CONSOLE_FILES) >> $@
	@echo "Wrote " $@

node_modules/biwascheme/lib/biwascheme.js: src/platforms/node/module_preamble.js lib/console_biwascheme.js src/platforms/node/module_postamble.js
	cat src/platforms/node/module_preamble.js > $@
	cat lib/console_biwascheme.js >> $@
	cat src/platforms/node/module_postamble.js >> $@
	@echo "Wrote " $@
