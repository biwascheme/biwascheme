default: build

build: lib/biwascheme.js

lib/biwascheme.js: src/dumper.js src/extra_lib.js src/io.js src/prototype.js src/r6rs_lib.js src/stackbase.js src/webscheme_lib.js
	cat src/prototype.js src/stackbase.js src/r6rs_lib.js  src/webscheme_lib.js src/extra_lib.js src/dumper.js src/io.js > __merged.js
	yuicomp __merged.js -o lib/biwascheme.js
	rm __merged.js
	echo "Wrote lib/biwascheme.js"
