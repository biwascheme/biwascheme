#
# Makefile - gather javascripts and compress it
#
# Needs YUI Compressor and a shellscript 'yuicomp' like:
#  #!/bin/sh
#  java -jar /somewhere/yuicompressor-2.4.2/build/yuicompressor-2.4.2.jar $*

FILES = \
  src/console/web-console.js \
  src/version.js \
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

CONSOLE_FILES =					\
  src/version.js				\
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


all: build

build: lib/biwascheme.js lib/console_biwascheme.js

lib/biwascheme.js: $(FILES) Makefile
	cat $(FILES) > __merged.js
	yuicomp __merged.js -o $@
	rm __merged.js
	@echo "Wrote " $@

lib/console_biwascheme.js: $(CONSOLE_FILES) Makefile
	cat $(CONSOLE_FILES) > __merged.js
	yuicomp __merged.js -o $@
	rm __merged.js
	@echo "Wrote " $@
