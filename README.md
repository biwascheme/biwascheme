BiwaScheme
==========

[![biwascheme logo](http://www.biwascheme.org/website/images/biwascheme_logo.png)](http://www.biwascheme.org)

BiwaScheme is a Scheme interpreter written in JavaScript.

Works with web browsers (including mobile devices) and Node.js.

Demos
-----

see [http://www.biwascheme.org/](http://www.biwascheme.org/)

Download
========

* https://github.com/biwascheme/biwascheme/raw/master/release/biwascheme-0.6.9.js (release version)

How to use
==========

Just load biwascheme.js (or biwascheme-min.js) and write Scheme code.

    <!DOCTYPE html>
    <html>
    <body>
    
    <div id="bs-console"></div>
    
    <script src="biwascheme.js">
    (display "hello, world!")
    </script>
    
    </body>
    </html>

How to use with node.js to run a biwa script
--------------------------------------------

1. $ npm install biwascheme
2. create a file a.scm:

    (display "Hello, world!")
    (newline)

3. $ biwas a.scm

How to use from inside node.js as a module
------------------------------------------

1. $ npm install biwascheme
2. create a file server.js:

    var BiwaScheme = require("biwascheme");
    BiwaScheme.run("(+ 1 2)");
    // or
    // BiwaScheme.run_file("a.scm");

3. $ node server.js

Files
=====

* release/
  + the following files are generated here with $ make
    - biwascheme.js
    - biwascheme-min.js
    - node_biwascheme.js
* demo/
  + Demos
* src/
  + deps/
     - Dependencies (jQuery, underscore)
  + system/
     - Source code of the interpreter
  + library/
     - Built-in library functions
  + platform/
     - Platform dependent code (browser, node, etc.)
* test/
  + Unit tests
* tuplespace/
  + (experimental) TupleSpace implemented in Scheme
* web/
  + HTMLs and CSS of www.biwascheme.org
* www.biwascheme.org.js
  + web server

Building biwascheme.js
----------------------

Prerequisites:

* make
* sed
* node (Node.js)
* uglifyjs ($ npm install uglify-js -g) 

Make compiles src/\*.js into release/biwascheme.js.

    $ make

Links
=====

- [Repository](https://github.com/biwascheme/biwascheme)
- [Issues](https://github.com/biwascheme/biwascheme/issues)
- [Mailing-list](http://groups.google.co.jp/group/biwascheme)
- [Mailing-list(Japanese)](http://groups.google.co.jp/group/biwascheme-ja)

Development memos
=================

How to add a new file
---------------------

* edit Makefile
* edit src/development_loader.js

How to release
--------------

(moved to doc/dev/release.md)

How to upgrade dependencies
---------------------------

jQuery:
* update src/deps/jquery.js

underscore:
* update src/deps/underscore.js
* update node_modules/biwascheme/package.json

underscore.string:
* update src/deps/underscore.string.js
* update node_modules/biwascheme/package.json

Website
-------

The source of www.biwascheme.org is in `./website`.

### Run local website

You can run the website locally with Node and express.

    $ make
    $ make website
    $ npm install
    $ node local_website.js
    $ open http://localhost:7001

License
=======

MIT-LICENSE

BiwaScheme logo by [@jcubic](https://github.com/jcubic): [Creative Commons Attribution 3.0](http://creativecommons.org/licenses/by/3.0/)

Acknowledgements
================

* Kent Dyvbig, Three implementation models for scheme
  * http://www.cs.indiana.edu/~dyb/pubs/3imp.pdf

* jsScheme
  * http://alex.ability.ru/scheme.html (inavailable)

* ExplorerCanvas (demo/excanvas.js)
  * http://excanvas.sourceforge.net/

Contact
=======

https://github.com/biwascheme/biwascheme

Yutaka HARA (yhara) yutaka.hara.gmail.com
http://twitter.com/yhara_en
