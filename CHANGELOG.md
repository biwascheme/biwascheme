## 2021-03-19 v0.7.2

Possible breaking change

- To surpress rollup's warning, `js-eval` now evaluates the argument in the global scope (#233)
- For example, `(js-eval "require('fs')")` now results in ReferenceError. Please use `(node-require "fs")` instead

Other changes

- feat: Npm package now includes biwascheme.mjs (#230)
- feat: New function `node-require` (#233)
- fix: `element-read-attribute` of v0.7.1 returns different value from v0.7.0 (#212)
- dev: Removed `local_website.js`. Use `npm run serve` instead

## 2020-12-29 v0.7.1

Possible breaking changes

- On introducing rollup (#160), some of the properties of `window.BiwaScheme`
  that are considered internal are made private (For example, `BiwaScheme.Class`).
  If you have a program which relies on any of them, please file an issue
  and I'll make it public again.
- Updated bundled dependencies.
  - Underscore.js 1.2.2 -> 1.10.2
  - jQuery 1.7.1 -> 3.5.1

Other changes

- feat: parameterize (#188)
- feat: BiwaScheme.Parser.parse
- fixes: #136 #197 #198 #204

## 2020-03-26 v0.7.0

  * new: Arity check for Scheme closures (#132)
  * new: dynamic-wind (#154)
  * new: biwascheme_terminal got some features like syntax highlighting (#133, #135)
  * new: `date->string` supports V (week num) (#134)
  * fix: Allow an empty list of bindings in let* (#131)
  * fix: encoding error on `load` (#138)
  * fix: error on tail position call/cc (#152)
  * dependency: Update underscore and underscore.string

## 2018-12-17 v0.6.9

  * new: Vector quasiquote
  * new: Allow `(string)` (#120)
  * fix: Fixed nested quasiquote (#100)
  * fix: `(vector)` was not allowed
  * fix: `(read)` should be a async function (#106, #113)
    - eg. `(define x (read))` now works on Node repl (./bin/biwas) and the [website repl](https://www.biwascheme.org)
  * fix: `string-number` improved (#119)
  * fix: `log` did not work (#120)
  * fix: Allow `(append ls)` (#120)
  * fix: Backtrace get longer with every error on an Interpreter object (#122)

## 2017-07-21 v0.6.8

  * new: js-new can take JS constructor function (#97)
  * fix: `(eval 123)` raises error (#83)
  * fix: "XML Parsing Error" on Firefox (fix #88)
  * fix: error on i.html (#93)
  * fix: (Node.js) `biwas` freeze with `#(1 2)` (#21)
  * fix: (Node.js) `(read)` may conflict with `display` (#84)
  * fix: (Node.js) `load` and `js-load` find source file from the current directory (#89)
  * fix: (Node.js) file-exists?, delete-file, get-environment-variable did not work

## 2016-12-25 v0.6.7

  * new: list-sort, vector-sort, vector-sort! now can take a comparator proc (#77, #78)
  * new: case-lambda (#81)
  * new: <script type="text/biwascheme"> also interpreted as Scheme program (#82)
  * fix: surpress deprecation warning for util.print on Node.js
  * fix: `BiwaScheme.define_scmfunc` was broken (#76)
  * fix: fixed error on `load` on Firefox (#79)
  * fix: element-toggle-class-name!, etc. shows deprecation warning unintentionally (#80)

## 2016-10-06 v0.6.6

  * new: string->number supports complex numbers (#72)
  * fix: fixed number predicates (eg. (complex? 1) now returns #t) (#70)
  * fix: fixed "BiwaScheme.Port.current_error.puts is not a function" (#75)
  * fix: error on defining a value based on itself (#74)
      ; eg.
      (define a 1)
      (define a (+ a 1))

## 2016-09-15 v0.6.5

  * new: complex numbers arithmetics (#69)
  * new: `readline` is supported on Node (#23)

## 2016-05-31 v0.6.4

  * new: R7RS promise (delay, force, delay-force, etc.)
  * change:
    - Add BiwaScheme.VERSION (BiwaScheme.Version is still available
      but deprecated)
    - `wait-for` returns event object instead of `#<undef>`

## 2015-12-25 v0.6.3

  * new:
    - macro `..` (expands to `js-invocation`)
  * change:
    - added `element-write-attribute!`, `element-add-class-name!`,
      `element-remove-class-name!`, `element-toggle-class-name!`
      and deprecated these without `!`
    - added `list->js-array`, `js-array->list`,
      `js-obj->alist`, `alist->js-obj` and deprecated
      `list-to-js-array`, etc.
  * Add reference http://www.biwascheme.org/doc/reference.html
  * Some bug fixes

## 2014-02-16  v0.6.2

  * change:
      - upgraded from jQuery 1.6.2 to 1.7.1
      - switched inspected strings to be double-quoted instead of single-quoted
      - define-record-type was defining mutator with wrong name like
        'set-book-name!', but fixed to define 'book-name-set!'
      - scoped assertion helpers to BiwaScheme (assert_string, etc)

  * new:
      - added stack traces to exceptions (GH #9)
      - Support hex character literals (eg. #\x41 => #\A) (GH #17)
      - $ and getelem functions can now take 1 or 2 arguments like in jQuery (second optional argument is the scope to search for the selector in)
      - functions and syntaxes:
        o element-has-class-name?, element-add-class-name (fixed),
          element-remove-class-name, element-toggle-class-name
          element-select
        o receive
        o remove-handler! (also, add-handler! now returns a reference to the handler function so that it can be passed to remove-handler!)
        o bitwise-* 
        o regexp-replace-all
        o vector-push! (= Array#push in JS)
      - Node:
        o js-load
        o BiwaScheme.run_file
        o underscore.js is available at BiwaScheme._, underscore.string at BiwaScheme._.str
        o srfi-98 get-environment-variable(s)

  * some bug fixes

## 2011-12-01  v0.6.1

  * fix: bin/biwas works on Node 0.6.x

  * change: do not raise error when null or undefined is returned
            from libfunc

  * new:
      - R6RS Enumerations
      - mutable hashtables
      - new functions
        - format (srfi-28)
        - write-char, with-output-to-port, call-with-string-output-port

  * new on Node:
    - One line program on command-line
      $ biwas -e "(print 'hello)"
    - REPL
      $ biwas   # (no arguments)
    - file-exists?, delete-file, command-line, exit
        (R6RS standard library chapter 9 and 10)

## 2011-11-22  v0.6.0

  * change: migrated from prototype.js to jQuery and underscore.js

  * change: argument for $ is passed to jQuery's $.
      old: ($ "foo")
      new: ($ "#foo")

  * new functions:
      list-copy
      console-log, console-debug, console-info,
        console-warn, console-error
      inc!, dec!
      dotimes
      js-function?

  * new demos:
      demo/raphaeljs/
      demo/chat_with_socketio_and_node/

## 2011-08-07  v0.5.7

  * new: install via npm ($ npm install biwascheme)
    it means you can do require('biwascheme') on Node
  * improve: added tuplespace/README
  * fix: element-write-attribute did not work

## 2011-07-07  v0.5.6

  * new: Node.js support! (try 'node bin/biwas a.scm')

## 2011-06-30  v0.5.5.2

  * new: list-sort/comp
  * fix: functions with multiple body exprs did not work

## 2010-12-25  v0.5.5.1

  * new: R6RS Records
  * bug fixes

## 2010-08-27  v0.5.4.2

  * fix: (let-values)
  * fix: (acos)
  * new: (gensym) returns random symbol
  * new: (string->number) parses flonum
  * new: BiwaScheme.GitCommit
    note: now sed is needed to compile biwascheme.js
