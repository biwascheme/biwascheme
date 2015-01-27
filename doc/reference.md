## List of functions

### Basic types

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

* Integer (1, -2, etc.)
  * `integer?` `odd?` `even?` `random-integer`
* Rational (literal not supported yet)
  * `rational?`
* Real (1.2, -2.3, etc.)
  * `real?` `random-real`
* Complex // literal not supported yet
  * `complex?` `make-rectangular` `make-polar` `real-part` `imag-part` `magnitude` `angle`
* Number
  * `number?` `=` `<` `>` `<=` `>=` `zero?` `positive?` `negative?`
  * `finite?` `infinite?` `nan?`
  * `max` `min` `+` `-` `*` `/` `abs` `div0-and-mod0` `div` `mod` `div0-and-mod0` `div0` `mod0`
  * `numerator` `denominator` `floor` `ceiling` `truncate` `round` `(rationalize)`
  * `exp` `log` `sin` `cos` `tan` `asin` `acos` `atan` `sqrt` `exact-integer-sqrt` `expt`
  * `number->string` `string->number`
  * `(random-integer n)` => 0..n-1 (srfi-27)
  * `(random-real)` => 0.0...1.0 (srfi-27)

### Control structure

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

<a name="macro">

BiwaScheme does not have `syntax-ruels` or `syntax-case`, but has `define-macro`.

```
(define-macro (test expr)
  `(if ,expr
    #t
    (print (format "test failed: ~a" ,expr))))

(test (= 1 2)) ;=> test failed: (= 1 2)
```

Other macro-related functions:

* `macroexpand` `macroexpand-1` `%macroexpand` `%macroexpand-1` `gensym`

### Advanced types

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

### Utilities

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

* Comment
  * Single line comment (;)
  * Multi line comment (#| ... |#)
  * One sexp comment (#;)
* Quote
  * quote (')
  * quasiquote (\`) unquote (,) unquote-splicing (,@)

### JavaScript language interface

* `(js-eval str)` evaluate `str` as JavaScript code
* `(js-ref jsobj str)` = `a.b`
* `(js-set! jsobj str value)` = `a.b = c`
* `(js-call jsfunc args...)` = `a()`
* `(js-invoke jsobj methodname args...)` = `a.b()`
* `js-invocation`
* macro `..`
  * TODO: write doc

* `(js-new ctorname args...)` = `new a`
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

### System functions (Node.js only)

* R6RS
  * `(file-exists? filepath)` 
  * `(delete-file filepath)` 
  * `(command-line)`
  * `(exit)` `(exit num)`

* srfi-98
  * `(get-environment-variable name)` -> string or #f
  * `(get-environment-variables)` -> alist of string (("key" . "value"))

You can also use Node.js libraries via `js-eval`, etc.

```
(define fs (js-eval "require('fs')"))

(define path ".")
(print (js-invoke fs 'readdirSync path))
; Alternatively you can use `..` macro syntax:
; (print (.. fs `(readdirSync ,path)))
```
