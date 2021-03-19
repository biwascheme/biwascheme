# BiwaScheme

[![biwascheme logo](http://www.biwascheme.org/website/images/biwascheme_logo.png)](http://www.biwascheme.org)

BiwaScheme is a Scheme interpreter written in JavaScript.

Works with web browsers (including mobile devices) and Node.js.

## Demos

see [http://www.biwascheme.org/](http://www.biwascheme.org/)

## Download

* http://www.biwascheme.org/

(Or you can just copy the file from [`./release`](release/).)

## How to use

Just load biwascheme.js (or biwascheme-min.js) and write Scheme code.

```html
<!DOCTYPE html>
<html>
<body>

<div id="bs-console"></div>

<script src="biwascheme.js"></script>
<script type="text/biwascheme">
(display "hello, world!")
</script>

</body>
</html>
```

### How to use with node.js to run a biwa script

1. $ npm install -g biwascheme
2. create a file a.scm:

    (display "Hello, world!")
    (newline)

3. $ biwas a.scm

### How to use from inside node.js as a module

1. $ npm install biwascheme
2. create a file a.js:

    var BiwaScheme = require("biwascheme");
    BiwaScheme.run("(+ 1 2)"); // or BiwaScheme.run_file("a.scm");

3. $ node a.js

### Statically build biwa-powered website

The npm package contains `biwascheme.mjs` for module bundlers like [rollup](https://rollupjs.org/guide/en/). See https://github.com/acmiyaguchi/svelte-biwascheme-example for an example.

Note that biwascheme.mjs does not include jQuery unlike biwascheme-x.y.z.js.
You need to bundle jQuery manually if you want to use functions defined in
src/library/webscheme_lib.js.

### Building biwascheme.js

See [doc/dev/build.md](doc/dev/build.md).

## Conformance

BiwaScheme [implements](https://www.biwascheme.org/doc/features.html) most of the features of [R7RS small](https://small.r7rs.org/), including first-class continuation and tail call optimization.

Major lacking features are:

- syntax-rules
- Exceptions
- Library system

There are two limitations that arise from JavaScript. These will not be fixed (it's technically possible to fix but will be very inefficient).

- Strings are immutable
- Integers are not distinguished from float

## Links

- [Repository](https://github.com/biwascheme/biwascheme)
- [Issues](https://github.com/biwascheme/biwascheme/issues)
- [Mailing-list](http://groups.google.co.jp/group/biwascheme)
- [Mailing-list(Japanese)](http://groups.google.co.jp/group/biwascheme-ja)

## License

MIT-LICENSE

BiwaScheme logo by [@jcubic](https://github.com/jcubic): [Creative Commons Attribution 3.0](http://creativecommons.org/licenses/by/3.0/)

## Acknowledgements

* Kent Dyvbig, Three implementation models for scheme
  * http://www.cs.indiana.edu/~dyb/pubs/3imp.pdf

* jsScheme
  * http://alex.ability.ru/scheme.html (inavailable)

* ExplorerCanvas (demo/excanvas.js)
  * http://excanvas.sourceforge.net/

## Contact

https://github.com/biwascheme/biwascheme

Yutaka HARA (yhara) yutaka.hara.gmail.com
http://twitter.com/yhara_en
