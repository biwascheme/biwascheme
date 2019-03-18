## List of functions

* [Basic types](#basic)
* [Numeric types](#numeric)
* [Control structure](#control)
* [Macro](#macro)
* [Advanced types](#advanced)
* [Utilities](#utilities)
* [Syntax](#syntax)
* [JavaScript language interface](#js-interface)
* [Browser functions](#browser)
* [Node.js](#nodejs)
* [BiwaScheme JavaScript API](#js-api)

### Basic types

<a name="basic"/>

* List
  * `length` `append` `reverse` `list-tail` `list-ref` `map` `for-each`
  * `find` `for-all` `exists` `filter` `partition` `fold-left` `fold-righ`
  * `remp` `remove` `remv` `remq`
  * `memp` `member` `memv` `memq`
  * `assp` `assoc` `assv` `assq`
  * `cons*`
  * `list-sort`
  * `(iota count start? step?)` (srfi-1)
  * `(list-copy list)` (srfi-1)
* String
  * `string?` `make-string` `string` `string-length` `string-ref`
  * `string=?` `string<?` `string>?` `string<=?` `string>=?`
  * `substring` `string-append` `string->list` `list->string`
  * `string-for-each` `string-copy`
  * `(open-input-string s)` (srfi-6)
  * `(open-output-string)` (srfi-6)
  * `(get-output-string strport)` (srfi-6)
* Vector
  * `vector?` `vector` `vector-length` `vector-ref` `vector-set!`
  * `vector->list` `list->vector` `vector-fill!` `vector-map` `vector-for-each`
  * `(vector-append v1 v2...)` `(vector-copy v)` (srfi-43)
  * `vector-sort` `vector-sort!`
* Boolean
  * `boolean?` `boolean=` `not` `and` `or`
* Pair
  * `pair?` `null?` `list?`
  * `cons` `list` `car` `cdr` `set-car!` `set-cdr!`
  * `caar` `cadr` `...` `cdddar` `cddddr`
* Symbol
  * `symbol?` `symbol->string` `symbol=?` `string->symbol`
* Char
  * `char?` `char->integer` `integer->char` `char=?` `char<?` `char>?` `char<=?` `char>=?`

### Numeric types

<a name="numeric"/>

* Integer (1, -2, etc.)
  * `integer?` `odd?` `even?`
  * `(random-integer n)` => 0..n-1 (srfi-27)
* Rational (literal not supported yet)
  * `rational?`
* Real (1.2, -2.3, etc.)
  * `(random-real)` => 0.0...1.0 (srfi-27)
* Complex // literal not supported yet
  * `complex?` `make-rectangular` `make-polar` `real-part` `imag-part` `magnitude` `angle`
* Number
  * `number?` `=` `<` `>` `<=` `>=` `zero?` `positive?` `negative?`
  * `finite?` `infinite?` `nan?`
  * `max` `min` `+` `-` `*` `/` `abs` `div0-and-mod0` `div` `mod` `div0-and-mod0` `div0` `mod0`
  * `numerator` `denominator` `floor` `ceiling` `truncate` `round` `(rationalize)`
  * `exp` `log` `sin` `cos` `tan` `asin` `acos` `atan` `sqrt` `exact-integer-sqrt` `expt`
  * `number->string` `string->number`

### Control structure

<a name="control"/>

* Conditional
  * `if` `when` `unless` `cond` `case`
* Loop
  * `do` `let(named` `let)` `dotimes`
* Exception
  * `raise`
* Assignment
  * `let` `let1` `let\*` `letrec` `letrec\*`
* Values
  * `values` `call-with-values` `let-values` `let\*-values`
  * `(receive <formals> <expression> <body>...)` (srfi-8)
* `apply`
* `eval`
* `eq?` `eqv?` `equal?`

### Macro

<a name="macro"/>

BiwaScheme does not have `syntax-rules` or `syntax-case`, but has `define-macro`.

```
(define-macro (test expr)
  `(if ,expr
    #t
    (print (format "test failed: ~a" (quote ,expr)))))

(test (= 1 2)) ;=> test failed: (= 1 2)
```

Other macro-related functions:

* `macroexpand` `macroexpand-1` `%macroexpand` `%macroexpand-1` `gensym`

### Advanced types

<a name="advanced"/>

* Port (I/O)
* Record
  * `define-record-type`
* Hashtable
  * `make-eq-hashtable` `make-eqv-hashtable` `make-hashtable`
  * `equal-hash` `string-hash` `string-ci-hash` `symbol-hash`
  * `hashtable?` `hashtable-size` `hashtable-ref` `hashtable-set!`
  * `hashtable-delete!` `hashtable-contains?` `hashtable-update!`
  * `hashtable-copy` `hashtable-clear!` `hashtable-keys` `hashtable-entries`
  * `hashtable-equivalence-function` `hashtable-hash-function` `hashtable-mutable?`
* Enumeration
  * `define-enumeration`
  * `enum-set-member?` `enum-set-subset?` `snum-set=?` `enum-set->list`
  * `enum-set-union` `enum-set-intersection` `enum-set-difference`
     `enum-set-complement` `enum-set-projection`
* Date (srfi-19)
  * `(current-date [tz-offset])`
    * Note: tz-offset is currently ignored and always local time is returned
    * Note: If you need a JavaScript `Date` object, use `js-eval` or 
      `(js-new "Date")`.
  * `(date? x)` `(date-nanosecond d)` `(date-millisecond d)`
    `(date-second d)` `(date-minute d)` `(date-hour d)` `(date-day d)`
    `(date-month d)` `(date-year d)` `(date-week-day d)`
      * Note: `date-month` returns 1..12, not 0..11
  * `(date->string date [template])`
    * eg. `(date->string (current-date) "~Y-~m-~d ~H:~M:~S")`
    * See [srfi-19](http://srfi.schemers.org/srfi-19/srfi-19.html) 
      for full list of `~?`
  * `(parse-date date)` = `Date.parse`
* Regexp
  * `regexp-exec` `(regexp-exec)` `(regexp-replace-all)`
  * `string->regexp` `regexp?` `regexp->string`
* Promise (R7RS)
  * syntax `(delay expression)`
    - Return a promise
  * syntax `(delay-force promise-expr)`
    - Return a promise
    - `promise-expr` must evaluates to a promise 
    - Mostly the same as `(delay (force promise-expr))`, but runs effectively
  * `(force promise)`
    - If no value has been computed for the promise, a value is computed and returned.
    - If the promise is forced a second time, the cached value is returned.
  * `(promise? obj)`
    - Return #t if obj is a promise
  * `(make-promise obj)`
    - Return a promise which, when `force`d, will return obj
    - Just return obj if obj is already a promise

### Utilities

<a name="utilities"/>

* Simple format (srfi-28)
  * eg. `(format "Result: ~s" (some-calculation))`
  * `(format format-str obj1 obj2 ...)` -> string
  * `(format #f format-str ...)` -> string
  * `(format #t format-str ...)` -> output to current port 
  * `(format port format-str ...)` -> output to the port 
      * `~a`: display
      * `~s`: write
      * `~%`: newline
      * `~~`: tilde
* Write with shared structure (srfi-38)
  * `(write/ss x)`
  * alias: `write-with-shared-structure`, `write*`

### Syntax

<a name="syntax"/>

* Comment
  * Single line comment (`;`)
  * Multi line comment (`#| ... |#`) (srfi-30)
  * One sexp comment (`#;`) (srfi-62)
* Quote
  * quote (`'`)
  * quasiquote (\`) unquote (`,`) unquote-splicing (`,@`)

### JavaScript language interface

<a name="js-interface" />

* `(js-eval str)` evaluate `str` as JavaScript code
* `(js-ref jsobj str)` = `a.b`
* `(js-set! jsobj str value)` = `a.b = c`
* `(js-call jsfunc args...)` = `a()`
* `(js-invoke jsobj methodname args...)` = `a.b()`
* `js-invocation`
* macro `..`
  * TODO: write doc

* `(js-new ctor args...)` = `new a`
  * eg. `(js-new (js-eval "Date") 2017 1 2)`
* `(js-new ctorname args...)` = `new a`
  * `ctorname` is evaluated as JavaScript program.
  * eg. `(js-new "Date" 2008 1 2)`
* `(js-obj key1 value1 key2 value2...)
  * eg. `(js-obj "foo" 1 "bar" 2)` → `{"foo": 1, "bar": 2}`
* `(js-closure proc)` returns a JS function that executes a Scheme function `proc`

* Predicates
  * `(js-null? x)`
  * `(js-undefined? x)`
  * `(js-function? x)`

* Conversion
  * `(js-array->list)`
  * `(list->js-array)`
  * `(js-obj->alist)`
  * `(alist->js-obj)`

* Timer
  * `(timer proc sec)` = setTimeout
  * `(set-timer! proc sec)` = setInterval
  * `(clear-timer! timer-id)` = clearInterval
  * `(sleep sec)`
      * Blocking sleep. `(sleep 3)` will wait 3 seconds and then 
        BiwaScheme resumes running the rest of the code. 

* Console
  * `(console-debug obj1 ...)` = console.debug
  * `(console-log obj1 ...)`
  * `(console-info obj1 ...)`
  * `(console-warn obj1 ...)`
  * `(console-error obj1 ...)`

### Browser functions

<a name="browser" />

These functions are only available in browsers (i.e. does not work on Node.js.)

* Dialog
  * `(alert msg)`
    * = window.alert
  * `(confirm msg)` => boolean
    * = window.confirm

* Element
  * `(element-empty! elem)` = `(element-clear! elem)`
  * `(element-visible? elem)`
  * `(element-toggle! elem)`
  * `(element-hide! elem)`
  * `(element-show! elem)`
  * `(element-remove! elem)`
  * `(element-update! elem html)`
  * `(element-replace! elem x)`
  * `(element-insert! elem x)`
  * `(element-select elem)`
  * `(element-read-attribute elem attr)`
  * `(element-write-attribute! elem attr value)`
  * `(element-height elem)`
  * `(element-width elem)`
  * `(element-has-class-name? elem class)`
  * `(element-add-class-name! elem class)`
  * `(element-remove-class-name! elem class)`
  * `(element-toggle-class-name! elem class)`
  * `(element-dimensions elem)` => (values width height)
  * `(element-focus! elem)`
  * `(element-content elem)` => string (html content or input value)
  * `(element-append-child! elem child)`
  * `(element-new spec`)

```
  (element-new '(div "foo"))        => <div>foo</div>
  (element-new '("div#main" "foo")) => <div id='main'>foo</div>
  (element-new '("div.red" "foo"))  => <div class='red'>foo</div>
  (element-new '(div align "right" "foo"))  => <div align='right'>foo</div>
  (element-new '(div (span "foo"))  => <div><span>foo</span></div>
```

* `(getelem selector)` = `($ selector)`
  * Same as `jQuery(selector)` except returns `#f` when no element is found
* `(set-style! selector prop value)`
  * eg. `(set-style! "#box" "left" 300)`
* `(get-style selector prop)`
* `(set-content! selector text)` 
  * Replace content of `selector` with `text`
  * Newlines in `text` are replaced with `<br>`
  * Tabs in `text` are replaced with `&nbsp;&nbsp;&nbsp;`
* `(get-content selector)` => string

* Event
  * `(add-handler! selector event proc)` => js-handler
  * `(remove-handler! selector event js-handler)`
    * eg. `(define h (add-handler! "#button1" "click" (lambda (ev) ...)))`
    * eg. `(remove-handler! "#button1" "click" h)`
  * `(wait-for selector event)`
    * eg. `(wait-for "#button1" "click") (alert "pushed")`
    * Synchronously wait for the event
      (rest of the program will be evaluated after the event)

* Ajax
  * `(http-request path)` => string
    * eg. `(http-get "/foo")`
    * Synchronously issue GET request 
      (rest of the program will be evaluated after server responded)
  * `(http-post path params)` => string
    * eg. `(http-post "/foo" '(("x" . 1) ("y" . 2)))`
    * Synchronously issue POST request 
      (rest of the program will be evaluated after server responded)
  * `(receive-jsonp url)` => string
    * eg. `(receive-jsonp "http://server/x")` will issue GET to
      `http://server/x?callback=BiwaScheme.jsonp_receiver[123]`

* Load Scheme or JS file on the server
  * `(load path)`
    * (eg. `(load "lib/foo.scm")`)
    * Synchronously load the Scheme program
      (rest of the program will be evaluated after server responded)
  * `(js-load js-path check)`
    * (eg. `(js-load "lib/foo.js" "Foo")`)
    * Synchronously load the JavaScript file
      (rest of the program will be evaluated after server responded)
    * Implemented with `script` tag. The argument `check` is used
      for detecting the end of loading. In the above example,
      BiwaScheme waits until `window.Foo` is defined.

### Node.js

<a name="nodejs" />

#### System functions

* R6RS
  * `(file-exists? filepath)` 
  * `(delete-file filepath)` 
  * `(command-line)`
  * `(exit)` `(exit num)`

* srfi-98
  * `(get-environment-variable name)` -> string or #f
  * `(get-environment-variables)` -> alist of string (("key" . "value"))

#### Loading other files

* (load path)
  - Load a scheme source file
* (js-load path)
  - Load a javascript source file

`path` is relative to the current directory, unless it starts with `/` or
`/^\w:/` (i.e. `c:`, `d:`, etc.)

#### Using Node.js libraries

You can also use Node.js libraries via `js-eval`, etc.

```
(define fs (js-eval "require('fs')"))

(define path ".")
(print (js-invoke fs 'readdirSync path))
; Alternatively you can use `..` macro syntax:
; (print (.. fs `(readdirSync ,path)))
```

### BiwaScheme JavaScript API

<a name="js-api" />

#### Evaluate Scheme program

```js
<script type="text/javascript" src="biwascheme.js"></script>
<script type="text/javascript">
var onError = function(e){ console.error(e); }
var biwa = new BiwaScheme.Interpreter(onError)
biwa.evaluate("(+ 1 2)", function(result) {
  alert(result);  //=> 3
});
</script>
```

#### Define library function

You can write JavaScript library function with `BiwaScheme.define_libfunc`.

- 1st argument: function name
- 2nd argument: minimum number of arguments
- 3rd argument: maximum number of arguments (pass null for infinite)
- 4th argument: function body

```js
BiwaScheme.define_libfunc("add", 2, 2, function(ar){
  BiwaScheme.assert_integer(ar[0]);
  BiwaScheme.assert_integer(ar[1]);
  return ar[0] + ar[1];
});
```

See `src/library/*.js` for more examples.
