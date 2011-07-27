#
# Makefile - gather javascripts and compress it
#

FILES0 = \
  src/jquery.js \
  src/underscore.js \
  src/underscore.string.js \
  src/rename_underscore.js \
  src/console/web-console.js \
  src/system/class.js \
  src/stackbase.js \
  src/system/set.js \
  src/system/write.js \
  src/system/pair.js \
  src/system/value.js \
  src/system/symbol.js \
  src/system/char.js \
  src/system/number.js \
  src/system/port.js \
  src/system/record.js \
  src/system/hashtable.js \
  src/system/syntax.js \
  src/system/types.js \
  src/system/parser.js \
  src/system/compiler.js \
  src/system/pause.js \
  src/system/call.js \
  src/system/interpreter.js \
  src/library/infra.js \
  src/library/r6rs_lib.js \
  src/library/js_interface.js \
  src/library/webscheme_lib.js \
  src/library/extra_lib.js \
  src/library/srfi.js \
  src/dumper.js \
  src/release_initializer.js \
#  src/io.js \

CONSOLE_FILES0 =					\
  src/system/class.js               \
  src/stackbase.js				    \
  src/system/set.js				    \
  src/system/write.js				\
  src/system/pair.js				\
  src/system/value.js				\
  src/system/symbol.js				\
  src/system/char.js				\
  src/system/number.js				\
  src/system/port.js				\
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
  src/library/js_interface.js       \
  src/library/extra_lib.js			\
  src/library/srfi.js				\
  src/dumper.js					    \
  \
  $(NULL)


VERSION_FILE_IN =  src/version.js.in
VERSION_FILE    =  src/version.js 
FILES           =  $(VERSION_FILE) $(FILES0)
CONSOLE_FILES   =  $(VERSION_FILE) $(CONSOLE_FILES0)

all: build

build: lib/biwascheme.js lib/console_biwascheme.js bin/biwas biwa_node_module/biwascheme/lib/biwascheme.js

$(VERSION_FILE): $(VERSION_FILE_IN) $(FILES0) $(CONSOLE_FILES0) VERSION Makefile
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

bin/biwas: src/server/node_preamble.js src/console/node-console.js lib/console_biwascheme.js src/node_main.js
	echo '#!/usr/bin/env node' > $@
	cat src/server/node_preamble.js >> $@
	cat src/console/node-console.js >> $@
	cat lib/console_biwascheme.js >> $@
	cat src/node_main.js >> $@
	chmod +x $@
	@echo "Wrote " $@

biwa_node_module/biwascheme/lib/biwascheme.js: src/server/node_preamble.js lib/console_biwascheme.js src/server/node_module_postamble.js
	cat src/server/node_preamble.js >> $@
	cat lib/console_biwascheme.js >> $@
	cat src/server/node_module_postamble.js >> $@
	@echo "Wrote " $@
