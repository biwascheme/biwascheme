#
# Makefile - gather javascripts and compress it
#
# Needs YUI Compressor and a shellscript 'yuicomp' like:
#  #!/bin/sh
#  java -jar /somewhere/yuicompressor-2.4.2/build/yuicompressor-2.4.2.jar $*

FILES = src/version.js \
  src/prototype.js \
  src/stackbase.js \
  src/system/set.js \
  src/system/write.js \
  src/system/pair.js \
  src/system/value.js \
  src/system/symbol.js \
  src/system/char.js \
  src/system/port.js \
  src/system/syntax.js \
  src/system/parser.js \
  src/system/compiler.js \
  src/system/interpreter.js \
  src/library/infra.js \
  src/library/r6rs_lib.js \
  src/library/webscheme_lib.js \
  src/library/extra_lib.js \
  src/dumper.js \
  src/release_initializer.js \
#  src/io.js \

all: build

build: lib/biwascheme.js

lib/biwascheme.js: $(FILES)
	cat $(FILES) > __merged.js
	yuicomp __merged.js -o lib/biwascheme.js
	rm __merged.js
	echo "Wrote lib/biwascheme.js"
