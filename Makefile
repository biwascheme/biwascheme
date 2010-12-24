#
# Makefile - gather javascripts and compress it
#

FILES0 = \
  src/console/web-console.js \
  src/prototype.js \
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
  src/library/webscheme_lib.js \
  src/library/extra_lib.js \
  src/library/srfi.js \
  src/dumper.js \
  src/release_initializer.js \
#  src/io.js \

CONSOLE_FILES0 =					\
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
  src/library/extra_lib.js			\
  src/library/srfi.js				\
  src/dumper.js					\
  \
  $(NULL)


VERSION_FILE_IN =  src/version.js.in
VERSION_FILE    =  src/version.js 
FILES           =  $(VERSION_FILE) $(FILES0)
CONSOLE_FILES   =  $(VERSION_FILE) $(CONSOLE_FILES0)

all: build

build: lib/biwascheme.js lib/console_biwascheme.js

$(VERSION_FILE): $(VERSION_FILE_IN) $(FILES0) $(CONSOLE_FILES0) Makefile
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
