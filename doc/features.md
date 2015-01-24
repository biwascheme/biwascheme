## Features

* Most of basic features of R6RS Base
* Some of R6RS Standard Libraries and SRFIs
  (see [doc/conformance](/doc/conformance.html) )
* First-class continuation (`call/cc`)

### Macro

BiwaScheme does not have `syntax-ruels` or `syntax-case`, but has [`define-macro`](/doc/reference.html#macro).

### Javascript language interface

* JavaScript object
    * js-eval js-ref js-set! js-call js-invoke
    * js-new js-obj js-closure
    * js-null? js-undefined? js-function?
* macro `..`
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

* Browser functions
  * load js-load
  * http-request http-post receive-jsonp
  * alert confirm
  * add-handler! remove-handler!
  * set-style! get-style set-content! get-content

