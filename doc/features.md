## Features

* Most of basic features of R7RS
* Some of R6RS Standard Libraries and SRFIs
  (see [doc/conformance](/doc/conformance.html) )
* First-class continuation (`call/cc`)

See [doc/reference](/doc/reference.html) for full list of functions.

### Macro

BiwaScheme does not have `syntax-rules` or `syntax-case`, but has [`define-macro`](/doc/reference.html#macro).

### Javascript language interface

* JavaScript object
    * js-eval js-ref, etc.
* Timer
  * `(timer proc sec)` = setTimeout, etc.
  * `(sleep sec)`
      * Blocking sleep. `(sleep 3)` will wait 3 seconds and then 
        BiwaScheme resumes running the rest of the code. 
* Console
  * `(console-debug obj1 ...)` = console.debug

* Browser functions
  * DOM manipulation
  * Events
  * Ajax
  * load js-load

[more...](/doc/reference.html#js-interface)

### Synchronous interface

Some of the BiwaScheme library functions are implemented as synchronous
functions.

For example, `(sleep sec)` will stop evaluation of Scheme program
for `sec` seconds, and then the rest of the program will be evaluated.

```
(alert "Hello")
(sleep 3)
(alert "World")  ; Evaluated after 3 seconds
```

Other synchronous functions are listed below.

* `(http-request path)` => string
* `(http-post path params)` => string
* `(receive-jsonp url)` => string
* `(load path)`
* `(js-load js-path check)`
* `(wait-for selector event)`

You can get rid of JavaScript's "callback hell" using these APIs.

```
// JavaScript (nested callback)
$.get("/a", function(a){
  $.get("/b", function(b){
    $.get("/c", function(c){
      ...
    }
  }
}

// BiwaScheme
(define a (http-request "/a"))
(define b (http-request "/b"))
(define c (http-request "/c"))
...
```

### SRFIs

BiwaScheme implements some SRFIs ([Scheme Requests for Implementation](http://srfi.schemers.org/)).

* 1 List Utilities 
  * partially - `iota`
* 6 string port
* 8 receive
* 19 time 
  * partially
* 27 random 
  * partially - `random-integer`, `random-real`
* 28 simple `format`
* 30 multi-line comment (`#| ... |#`)
* 38 write/ss 
  * partially - `write/ss`
* 43 vector 
  * partially - `vector-append`
* 62 s-expr comment (`#;`)
* 98 get-environment-variable(s) 
  * Node.js only

### Node.js support

You can use BiwaScheme on the server-side with `npm install biwascheme`
([more...](/doc/getting_started.html#nodejs))
