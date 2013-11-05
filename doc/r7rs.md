# [R7RS](http://www.r7rs.org/)

BiwaScheme supports basic feature of R7RS, but currently not all.

The following list is based on r7rs.pdf (the final draft)
on the [Wiki](http://trac.sacrideo.us/wg/wiki#no1).

# 1 OverviewofScheme....................... 5
## 1.1 Semantics ......................... 5
## 1.2 Syntax........................... 5
## 1.3 Notationandterminology ................ 5
#2 Lexicalconventions ....................... 7
## 2.1 Identifiers ......................... 7
## 2.2 Whitespaceandcomments................ 8
## 2.3 Othernotations...................... 8
## 2.4 Datumlabels ....................... 9
# 3 Basicconcepts.......................... 9
## 3.1 Variables, syntactic keywords, and regions . . . . . . . 9
## 3.2 Disjointnessoftypes ................... 10
## 3.3 Externalrepresentations................. 10
## 3.4 Storagemodel....................... 10
## 3.5 Propertailrecursion ................... 11
# 4 Expressions ........................... 12

## 4.1 Primitiveexpressiontypes................ 12

* variable reference
* `quote`
* procedure call
* `lambda`
* `if`
* `set!`
* `include` `include-ci`
  * like `#include` in C

## 4.2 Derivedexpressiontypes................. 14

* `cond` `case` `and` `or` `when` `unless`
* [TODO]`cond-expand`
* `let` `let*` `letrec` `letrec*` `let-values`
* `begin`
* `do` named let
* [TODO]`delay`, `delay-force`, `force`, `promise?`, `make-promise`
* [TODO]`make-paremeter`, `parameterize`
* [TODO]`guard`
* quasiquote
* `case-lambda`

## 4.3 Macros........................... 21

* [TODO]`let-syntax`, `letrec-syntax`, `syntax-rules`, `syntax-error`

# 5 Programstructure........................ 25

## 5.1 Programs ......................... 25

## 5.2 Import declarations.................... 25

* [TODO]`import`

## 5.3 Variabledefinitions.................... 25

* `define`

## 5.4 Syntaxdefinitions .................... 26

* [TODO]`define-values`, `define-syntax`

## 5.5 Record-typedefinitions.................. 27

* [TODO]`define-record-type` (is it a subset of R6RS records?)

## 5.6 Libraries.......................... 28

* [TODO]`define-library`

## 5.7 TheREPL ........................ 29

# 6 Standardprocedures ...................... 30

## 6.1 Equivalencepredicates.................. 30

* `eqv?` `eq?` `equal?`

## 6.2 Numbers.......................... 32

* (number/complex/real/rational/integer)
* [TODO] `exact?` `inexact?` `exact-integer?` `exact` `inexact`
* [TODO] `#b`, `#o`, `#e`, `#i`
* `number?` `complex?` `real?` `rational?` `integer?`
* `finite?` `infinite?` `nan?` `=` `<` `>` `<=` `>=`
* `zero?` `positive?` `negative?` `odd?` `even?`
* `max` `min` `+` `*` `-` `/` `abs`
* [TODO] `floor/` `floor-quotient` `floor-remainder`
* [TODO] `truncate/` `truncate-quotient` `truncate-remainder`
* [TODO] `quotient` `remainder` `modulo`
* [TODO] `gcd` `lcm` `numerator` `denominator`
* `floor` `ceiling` `truncate` `round`
* [TODO] `rationalize`
* `exp` `log` `sin` `cos` `tan` `asin` `acos` `atan` 
* [TODO] `square`
* `sqrt` `expt`
* [TODO] `exact-integer-sqrt`
* `make-rectangular` `make-polar` `real-part` `imag-part` `magnitude` `angle`
* `number->string` `string->number`

## 6.3 Booleans.......................... 40

* `not` `boolean?` `boolean=`

## 6.4 Pairsandlists....................... 40

* `pair?` `cons` `car` `cdr` `set-car!` `set-cdr!`
* `caar` ... `cddddr`
* `null?` `list?` `make-list` `list` `length` `append`
* `reverse` `list-tail` `list-ref` `list-set!`
* `memq` `memv` `member` `assq` `assv` `assoc` 
* `list-copy`

## 6.5 Symbols.......................... 43

* `symbol?` `symbol=?` `symbol->string` `string->symbol`

## 6.6 Characters......................... 44

* [TODO] `#\alarm`, etc
* `char?` `char=?` `char<?` `char>?` `char<=?` `char>=?`
* `char-ci=?` `char-ci<?` `char-ci>?` `char-ci<=?` `char-ci>=?`
* [TODO] `char-alphabetic?` etc.
* [TODO] `digit-value`
* `char->integer` `integer->char`
* [TODO] `char-upcase` `char-downcase` `char-foldcase`

## 6.7 Strings........................... 45

* `string?` `make-string` `string` `string-length` `string-ref` `string-set!`
* `string=?` `string-ci=?`, etc.
* `string-upcase` `string-downcase`
* [TODO] `string-foldcase`
* `substring` `string-append` `string->list` `list->string` `string-copy`
* [TODO] `string-copy!` `string-fill!`

## 6.8 Vectors .......................... 48

* `vector?` `make-vector` `vector` `vector-length` `vector-ref`
* `vector-set!` `vector->list` `list->vector`
* `vector->string` `string->vector` `vector-copy`
* [TODO] `vector-copy!`
* `vector-append` `vector-fill!`

## 6.9 Bytevectors ........................ 49

* [TODO] `bytevector?` `make-bytevector` `bytevector`
  `bytevector-length` `bytevector-u8-ref` `bytevector-u8-set!`
  `bytevector-copy` `bytevector-copy!` `bytevector-append`
  `utf8->string` `string->utf8`

## 6.10Controlfeatures...................... 50

* `procedure?` `apply` `map` `string-map` `vector-map`
* `for-each` `string-for-each` `vector-for-each`
* `call/cc` `call-with-current-continuation`
* `values` `call-with-values`
* [TODO] `dynamic-wind`

## 6.11Exceptions ........................ 54

* [TODO] `with-exception-handler` `raise-continuable`
  `error` `error-object?` `error-object-message`
  `error-object-irritants` `read-error?` `file-error?`
* `raise`

## 6.12Environmentsandevaluation .............. 55

* [TODO] `environment` `scheme-report-environment`
  `null-environment` `interaction-environment`
* `eval`

## 6.13Inputandoutput..................... 55

* `call-with-port`
* [TODO] `call-with-input-file` `call-with-output-file`
* `port?` `input-port?` `output-port?` `textual-port?` `binary-port?`
* [TODO] `input-port-open? output-port-open?`
* `current-input-port` `current-output-port` `current-error-port`
* [TODO] `with-input-from-file` `with-output-to-file`
* [TODO] `open-input-file` `open-binary-input-file`
  `open-output-file` `open-binary-output-file`
* `close-port` `close-input-port` `close-output-port`
* `open-input-string` `open-output-string` `get-output-string`
* [TODO] `open-input-bytevector` `open-output-bytevector`
  `get-output-bytevector`

* `read`
* [TODO] `read-char` `peek-char` `read-line`
* `eof-object?` `eof-object`
* [TODO] `char-ready?`
* [TODO] `read-string` `read-u8` `peek-u8` `u8-ready?`
  `read-bytevector` `read-bytevector!`

* `write`
* [TODO] `write-shared` `write-simple`
* `display` `newline` `write-char`
* [TODO] `write-string` `write-u8` `write-bytevector` `flush-output-port`

## 6.14Systeminterface ..................... 59

* `load`
* `file-exists?` `delete-file` `command-line` `exit
* [TODO] `emergency-exit`
* [TODO] `get-environment-variable` `get-environment-variables`
* [TODO] `current-second` `current-jiffy` `jiffies-per-second`
* [TODO] `features`

# 7 Formalsyntaxandsemantics.................. 61
## 7.1 Formalsyntax....................... 61
## 7.2 Formalsemantics..................... 65
## 7.3 Derivedexpressiontypes................. 68
