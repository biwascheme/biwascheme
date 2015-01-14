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
* String
  * `string?` `make-string` `string` `string-length` `string-ref`
  * `string=?` `string<?` `string>?` `string<=?` `string>=?`
  * `substring` `string-append` `string->list` `list->string`
  * `string-for-each` `string-copy`
* Vector
  * `vector?` `vector` `vector-length` `vector-ref` `vector-set!`
  * `vector->list` `list->vector` `vector-fill!` `vector-map` `vector-for-each`
  * `vector-append` `vector-copy`
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
  * `values` `call-with-values` `let-values` `let\*-values` `receive`
* `apply`
* `eval`
* `eq?` `eqv?` `equal?`

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
* Date
  * `current-date`
  * `date?` `date-nanosecond` `date-millisecond` `date-second` `date-minute`
     `date-hour` `date-day` `date-month` `date-year` `date-week-day`
  * `date->string` `parse-date`
* Regexp
  * `regexp-exec` `(regexp-exec)` `(regexp-replace-all)`
  * `string->regexp` `regexp?` `regexp->string`

### Utilities

* `format`
* `write/ss`

### System functions (Node.js only)

* `file-exists?` `delete-file` `command-line` `exit`
* `get-environment-variable` `get-environment-variables`

### Syntax

* Comment
  * Single line comment (;)
  * Multi line comment (#| ... |#)
  * One sexp comment (#;)
* Quote
  * quote (')
  * quasiquote (\`) unquote (,) unquote-splicing (,@)


