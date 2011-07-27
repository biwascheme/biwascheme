#
# Makefile - gather javascripts and compress it
#

BASIC_FILES =					\
  src/prototype.js				\
  src/stackbase.js				\
  src/system/set.js				\
  src/system/write.js				\
  src/system/pair.js				\
  src/system/value.js				\
  src/system/symbol.js				\
  src/system/char.js				\
  src/system/number.js				\
  src/system/port.js				\
  src/system/record.js 	                        \
  src/system/hashtable.js			\
  src/system/syntax.js				\
  src/system/types.js				\
  src/system/parser.js				\
  src/system/compiler.js			\
  src/system/pause.js				\
  src/system/call.js				\
  src/system/interpreter.js			\
  src/library/infra.js				\
  src/library/r6rs_lib.js			\
  src/library/webscheme_lib.js                  \
  src/library/extra_lib.js			\
  src/library/srfi.js				\
  src/dumper.js

EXTRA_FILES = \
  src/console/web-console.js \
  src/release_initializer.js

VERSION_FILE_IN =  src/version.js.in
VERSION_FILE    =  src/version.js 
FILES           =  $(VERSION_FILE) $(BASIC_FILES) $(EXTRA_FILES)
CONSOLE_FILES   =  $(VERSION_FILE) $(BASIC_FILES)

all: build

build: lib/biwascheme.js lib/console_biwascheme.js biwa_node_module/biwascheme/lib/biwascheme.js

$(VERSION_FILE): $(VERSION_FILE_IN) $(BASIC_FILES) $(EXTRA_FILES) VERSION Makefile
	cat $< | sed -e "s/@GIT_COMMIT@/`git log -1 --pretty=format:%H`/" | sed -e "s/@VERSION@/`cat VERSION`/" > $@

lib/biwascheme.js: $(FILES) Makefile
	cat $(FILES) > __merged.js
	java -jar bin/yuicompressor-2.4.2.jar __merged.js -o $@
	rm __merged.js
	@echo "Wrote " $@

lib/console_biwascheme.js: $(CONSOLE_FILES) Makefile
	cat $(CONSOLE_FILES) > __merged.js
	java -jar bin/yuicompressor-2.4.2.jar __merged.js -o $@
	rm __merged.js
	@echo "Wrote " $@

biwa_node_module/biwascheme/lib/biwascheme.js: src/server/node_module_preamble.js lib/console_biwascheme.js src/server/node_module_postamble.js
	cat src/server/node_module_preamble.js >> $@
	cat lib/console_biwascheme.js >> $@
	cat src/server/node_module_postamble.js >> $@
	@echo "Wrote " $@
