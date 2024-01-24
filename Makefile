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

index.html: website/_header.html website/index_.html website/_footer.html
	sed -e  "s/@VERSION@/`node -e 'console.log(require("./package.json").version)'`/g" website/_header.html > $@
	sed -e  "s/@VERSION@/`node -e 'console.log(require("./package.json").version)'`/g" website/index_.html >> $@
	sed -e  "s/@VERSION@/`node -e 'console.log(require("./package.json").version)'`/g" website/_footer.html >> $@

website: index.html doc/**/*.md
	node bin/biwas tools/make_doc.scm

watch_website:
	ifchanged -d 'make website -B' doc/**/*.md doc/_*
