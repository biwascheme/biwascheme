#
# Makefile - gather javascripts and compress it
#

all: build

.PHONY: build
build:
	npm run build

#
# Test
#

browser_test:
	cd test/browser_functions; node ./server.js

#
# Website
#

index.html: website/_header.html index_.html website/_footer.html
	sed -e  "s/@VERSION@/`npm info biwascheme version`/g" website/_header.html >> $@
	sed -e  "s/@VERSION@/`npm info biwascheme version`/g" index_.html >> $@
	sed -e  "s/@VERSION@/`npm info biwascheme version`/g" website/_footer.html >> $@

website: index.html doc/**/*.md
	node bin/biwas tools/make_doc.scm

watch_website:
	ifchanged -d 'make website -B' doc/**/*.md doc/_*
