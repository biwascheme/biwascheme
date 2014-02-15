Intermediate Language
=====================

BiwaScheme is a VM based compiler-interpreter. Scheme program is translated into IL (Intermediate Language) by BiwaScheme.Compiler, and then executed by BiwaScheme.Interpreter.

Most of the opecodes are taken from 3imp.pdf.

Registers:

* a [object] : temporary
* x [opecodes] : opecodes to execute next
* f [integer] : [TODO: denotes start position of free variables in the stack?]
* c [closure] : closure object which is currently executed (set to [] in toplevel)
  * Closure object contains information about freevars (outer variables). When closure requires value of a free variable, it is retrieved from this register.
  * Used by op_refer-free, op_assign-free
* s [integer] : denotes stack size (TODO: can we remove this by using ary.push()/ary.pop()?)

Opecodes
--------

* Stack manipulation
  * [op_constant](#op_constant) : Set value of 'a' register
  * [op_argument](#op_argument) : Push value of 'a' register onto stack
* Control structure
  * [op_test](#op_test) : Conditional execution
  * [op_close](#op_close) : Create Scheme closure
  * [op_conti](#op_conti) : Capture continuation
  * [op_nuate](#op_nuate) : Invoke continuation
  * [op_halt](#op_halt) : Terminate program execution
* Function call
  * [op_frame](#op_frame) : Push stack frame
  * [op_apply](#op_apply) : Invoke procedure (Scheme closure or JS function)
  * [op_tco_hinted_apply](#op_tco_hinted_apply) : op_apply for tail call (Used for stack trace)
  * [op_return](#op_return) : Terminate execution of a procedure
  * [op_shift](#op_shift) : Quick stack frame discard for tail call optimization
* Variable reference/assignment
  * [op_refer-local](#op_refer-local)
  * [op_refer-free](#op_refer-free)
  * [op_refer-global](#op_refer-global)
  * [op_assign-local](#op_assign-local)
  * [op_assign-free](#op_assign-free)
  * [op_assign-global](#op_assign-global)
  * [op_box](#op_box) : Create a 'box' for variables which may be reassigned by set!
  * [op_indirect](#op_indirect) : Get the current value closed in a box

### op_constant
<a name="op_constant" />

Push val onto the stack. Often used with op_argument.

#### format

    ["constant", <value>, <opecodes_next>]

#### registers

* writes:
  * a: the opecode
  * x: next opecode

#### example

program:

```
(display "hello")
```

compiled:

```
[frame
   [constant "hello"
   [argument
   [constant 1
   [argument
   [refer-global "display"
   [apply]]]]]]
[halt]]
```

### op_argument
<a name="op_argument" />

Push a value (typically an argument of function call) onto the stack.

#### format

    ["argument", <opecodes_next>]

#### registers

* reads:
  * a: the value to push
  * s: old stack size
* writes:
  * x: next opecode
  * s: new stack size

#### example

program:

    (print "hi")

compiled:

    [frame
       [constant "hi"
       [argument
       [constant 1
       [argument
       [refer-global "print"
       [apply]]]]]]
    [halt]]

### op_test
<a name="op_test" />

Execute either of two operations depending on the value of 'a' register.

#### format

```
["test", <opecodes_then>, <opecodes_else>]
```

#### example

program:

```
(if 7 8 9)
```

compiled:

```
[constant 7
[test
   [constant 8
      [halt]]
   [constant 9
      [halt]]]]
```

### op_close
<a name="op_close" />

Create Scheme closure object (which is represented with JS Array).

#### format

```
["close", <n>, <body>, <opecodes_next>, <dotpos>]
```

TODO: &lt;opecodes_next> should be the last in consistency with other opecodes

#### description

op_close creates closure object onto the 'a' register.

* n
  * number of free variables (i.e. local variables defined outside) which this closure contains
  * eg. (lambda (a) a) -> 0
  * eg. (lamdba () (+ b c)) -> 2
* body
  * opecodes (compiled closure body)
* dotpos
  * (see below)

#### closure object

Scheme closures are represented by JS arrays with its attribute 'closure_p' assigned to true.

```
[body, freevar1, freevar2, ..., dotpos]
```

* body
  * closure body compiled into opecodes
* freevar
  * value of the free variable
  * If the free variable may be reassigned by set!, a box is stored instead of the direct value (see footnote)
* dotpos
  * used for handling rest args
  * (lambda args ...) : 0
  * (lambda (a . rest) ...) : 1
  * (lambda (a b . rest) ...) : 2
  * (lambda (a b c) ...) : -1
  * (lambda () ...) : 0

#### example

program:
```
(lambda () 1)
```

compiled:
```
[close 
  0
  [constant 1
    [return]]
  [halt]
  -1]
```

#### example of reassigned freevars

Usually closure object contains actual values of the freevars:

```
(define (f a b)
  (lambda () a b))
(f 11 22)

-> [ ["refer-free", 0, ["refer-free", 1, ["return"]]], 11, 22, -1]
```

But when the freevar may reassigned by set!, a box (JS array contains the actual value) is stored. It will be unboxed by op_indirect (see also: op_indirect, op_box)

```
(define (f a b)
  (set! a 99)
  (lambda () a b))
(f 11 22)

-> [ ["refer-free", 0, ["indirect", ["refer-free", 1, ["return"]]]], [99], 22, -1]
```

### op_conti
<a name="op_conti" />

Capture the current continuation.

Caputred continuation is just a Scheme closure which includes whole copy of current stack and op_nuate to restore it (see also: op_nuate).

#### format

```
["conti", <n>, <opecodes_next>]
```

n: number of args(for outer lambda) to remove (= 0 unless tail position)

#### example

program:
```
(call/cc func)
```

compiled:
```
[frame
   [conti 0
   [argument
   [constant 1
   [argument
   [refer-global "func"
   [apply]]]]]]
[halt]]
```

### op_nuate
<a name="op_nuate" />

Restore stack saved by op_conti (see op_conti).

#### format

```
["nuate", <saved_stack>, <opecodes_next>]
```

#### description

op_nuate takes saved stack as an argument. When op_nuate is invoked, the interpreter stack is replaced by this saved stack.

op_nuate is never generated by Compiler. It is dynamically generated by Interpreter when processing op_conti.

#### example

program:

```
(define cc (call/cc identity))
(cc)
```

compiled:

```
[refer-local 0
[nuate #<Object{stack,last_refer,call_stack,tco_counter}>
[return]]]
```

### op_halt
<a name="op_halt" />

Indicate end of program.

A program may have more than one op_halt (eg. `if` is compiled into op_test which contains then-clause and else-clause, both have op_halt on its end.)

#### example

program:

    '()

compiled:

    [constant ()
      [halt]]

### op_frame
<a name="op_frame" />

Push stack frame.

op_frame must be called before applying a function, except when it is a tail call (see also: op_shift.)

#### format

```
["frame", <opecodes_next>, <opecodes_after>]
```

* opecodes_after
  * opecodes executed after the new stack frame is popped from stack

#### description

op_frame pushes a frame (i.e. the following values) onto stack:

* <opecodes_after> - opecodes to execute after new frame is popped //stack top
* value of 'f' register - position of previous frame
* value of 'c' register - the closure currently executed

Usually this frame is popped by [return](#return).

op_frame is generated by:

* function calls (foo 1 2 3)
* call/cc (call/cc func)

#### example

program:
```
(print 11)
```

compiled:
```
[frame        // op_frame
   [constant 11          // push arguments to stack           
   [argument
   [constant 1           // push number of arguments 
   [argument
   [refer-global "print" // load function to 'a' register
   [apply]]]]]]          // invoke function
[halt]]
```

Note that op_frame itself does not touch 'f' register. Since function arguments are evaluated between op_frame and [apply](#apply) and these evaluations must be done in the current frame, 'f' register is updated by [apply](#apply).

### op_apply
<a name="op_apply" />

Invoke a function-like object, i.e. Scheme closure or BiwaScheme library JS function.

(Use `js-call` for native JS functions.)

#### format

```
["apply"]
```

#### description

In contrast to other opecodes, op_apply takes no arguments. Instead it retrieves needed information from the stack.

Before calling op_apply, you must put these values onto stack:

* func: closure or function to apply
* n_args: number of arguments
* arg1: first argument
* arg2: second argument
* arg3: third argument
* x // usually stack frame is already prepared by op_frame
* f
* c

##### applicable things

* Scheme closure
  * a JS array
* Library function (map, +, etc.)
  * a JS function which takes args(array) and intp(Interpreter) as arguments

Note that you cannot invoke pure JS functions directly. You can use js-call or js-invoke instead.

#### example

program:
```
(+ 11 22)
```

compiled:
```
[frame
   [constant 22  
   [argument          // push 22 (second argument) to stack
   [constant 11  
   [argument          // push 11 (first argument) to stack
   [constant 2   
   [argument          // push 2 (number of arguments) to stack
   [refer-global "+"  // load function '+' to 'a' register
   [apply]]]]]]]]     // invoke the function with arguments 11 and 22
[halt]]
```

### op_tco_hinted_apply
<a name="op_tco_hinted_apply" />

Same as op_apply, but used for tail calls. Added for stack trace.

#### format

```
["tco_hinted_apply"]
```

#### description

See op_apply for how it retrieves the funcion and its arguments.

#### Stack trace

BiwaScheme.Interpreter has three variables to handle stack trace:

* this.last_refer (String)
  - Name of the last variable referred by op_refer-global, etc. 
  - BiwaScheme IL does not contain names of local variables, so
    they are shown as "(anon)".

* this.call_stack (Array of String) 
  - Stack of function name

* this.tco_counter (Array of Integer)
  - Number of tail calls happened
  - tco_counter.length is equal to number of stack frames
    (Note that this is different from stack.length,
     because this.stack is one-dimentional array).
  - However, call_stack.length may not be the same as number of frames
    because tail call does not push stack frame.
  - tco_counter holds this difference, i.e. how many funcalls have occured
    without pushing stack frame.

Here is the difference between normal call and tail call.

- Normal funcall
  - op_frame pushes frame onto this.stack, pushes 0 onto tco_counter
  - op_apply pushes function name onto this.call_stack
  - op_return pops stack frame, tco_counter and call_stack
- Tail call
  - (op_frame is not called)
  - (op_shift is called)
  - tco_hinted_apply increments the value on top of tco_counter
  - op_apply pushes function name onto this.call_stack
  - op_return pops stack frame, tco_counter and call_stack.
    call_stack may be popped more than once if value of tco_counter >= 1.

Length of call_stack is capped to Interpreter#max_trace_size (which defaults
to BiwaScheme.max_trace_size). We do not cap length of tco_counter
because it does not grow with tail calls.

#### example

program:
```
(define (f) (print 7))
```

compiled:
```
[close 0                      // Create a Scheme closure
   [constant 7                // Body of f
   [argument
   [constant 1
   [argument
   [refer-global "print"
   [shift 1
   [tco_hinted_apply]]]]]]]   // Invoke print with tco_hinted_apply
   [assign-global "f"
   [halt]]
-1]
```

### op_return
<a name="op_return" />

Pop stack frame.

op_return is generated by (lambda ...) and must be called as the last opecode of function execution.

#### format

```
["return"]
```

op_return does not have <opecodes_next> because it pops next opecodes from stack.

#### description

op_return pops the following values from stack:

* n [integer] : number of arguments passed to this function call
* arg1
* arg2
* arg3
* x [opecodes] // stack frame pushed by op_frame
* f [integer]
* c [closure]

op_return also sets x, f, c to register 'x', 'f', 'c'.

#### example

program:
```
((lambda () 11))
```

compiled:
```
[frame
   [constant 0
   [argument
   [close 0
      [constant 11
      [return]]
      [apply]
   -1]]]
[halt]]
```

### op_shift
<a name="op_shift" />

Discard the arguments for parent call from stack.

Since stack frame is not pushed by op_frame and popped by op_return for tail calls, we need to remove old arguments with op_shift.

#### format

```
["shift", <n_args>, <opecodes_next>]
```

* n_args
  * number of arguments this tail call passes
* opecodes_next
  * must be ["apply"]

#### description

op_shift removes arguments for parent call from stack. Suppose the following code:

```
(define (foo x y)
  (bar "a" "b" "c"))
(foo "x" "y")
```

Before applying bar, op_shift removes the following values. Note that first four elements will not be popped - that's why this operation is named "shift".

* 3 //stack top
* "a" // arguments for tail call (bar)
* "b"
* "c"
* 2   // will be removed  // arguments for parent call (foo)
* "x" // will be removed
* "y" // will be removed

Also note that stack frame is not pushed by op_frame for tail calls.

#### example

program:
```
((lambda () (print 1))
```

Here (print 1) is a tail call.

compiled:
```
[frame
   [constant 0
   [argument
   [close 0  // make a closure, and
      [constant 1
      [argument
      [constant 1
      [argument
      [refer-global "print"
      [shift 1
      [apply]]]]]]]
      [apply]  // immediately invoke it
   -1]]]
[halt]]
```

Without tail call optimization, this program is compiled to:

```
[frame
   [constant 0
   [argument
   [close 0
      [frame  // push stack frame
         [constant 1
         [argument
         [constant 1
         [argument
         [refer-global "print"
         [apply]]]]]]
      [return]]  // pop stack frame
      [apply]
   -1]]]
[halt]]
```

### op_refer-local
<a name="op_refer-local" />

Load the value of a local variable.

#### format

```
["refer-local", <variable_pos>, <opecodes_next>]
```

#### example

program:
```
(define a 99)
```

compiled:
```
[frame
   [constant 7
   [argument
   [constant 1
   [argument
   [close 0
      [refer-local 0
         [return]]
      [apply]
      -1]]]]]
[halt]]
```

### op_refer-free
<a name="op_refer-free" />

Load the value of a free variable.

#### format

```
["refer-free", <variable_pos>, <opecodes_next>]
```

#### example

program:
```
(let ((a "a")(b "b"))
  (let ()
    a)
```

compiled:
```
[frame
   [constant "a"
   [argument
   [constant "b"
   [argument
   [constant 2
   [argument
   [close 0
      [constant 0
      [argument
      [refer-local 1
      [argument
      [close 1
         [refer-free 0
         [return]]
         [shift 0
         [apply]]
      -1]]]]]
      [apply]
   -1]]]]]]]
halt]
```

### op_refer-global
<a name="op_refer-global" />

Load the value of a certain global variable.

#### example

program:
```
map
```

compiled:
```
[refer-global "map"
[halt]]
```

### op_assign-local
<a name="op_assign-local" />

Assign value to a variable.

#### format

```
["assign-local", <FIXME>, <opecodes_next>]
```

#### example

See example of op_indirect.

### op_assign-free
<a name="op_assign-free" />

Assign a value to a free variable.

#### format

```
["assign-free", <FIXME>, <opecodes_next>]
```

#### example

program:
```
(let ((a "a"))
  (let ()
    (set! a "aa")))
```

compiled:
```
[frame
   [constant "a"
   [argument
   [constant 1
   [argument
   [close 0
      [box 0
      [constant 0
      [argument
      [refer-local 0
      [argument
      [close 1
         [constant "aa"
         [assign-free 0
         [return]]]
         [shift 0
         [apply]]
      -1]]]]]]
      [apply]
   -1]]]]]
halt]
```

### op_assign-global
<a name="op_assign-global" />

Assign a value to a global variable.

#### format

```
["assign-global", <str_variable_name>, <opecodes_next>]
```

#### example

program:
```
(define a 99)
```

compiled:
```
[constant 99
[assign-global "a"
[halt]]]
```

### op_box
<a name="op_box" />

Make a box for a variable which may be reassigned by set! (see op_indirect).

#### format

```
["box", <FIXME>, <opecodes_next>]
```

#### example

See example of op_indirect.

### op_indirect

is used to load the value of a variable which may reassigned by 'set!'.

#### format

```
["indirect", <opecodes_next>]
```

#### example

program:
```
(let ((a "a"))
  (set! a "b")
  a)
```

compiled:
```
[frame
   [constant "a"
   [argument
   [constant 1
   [argument
   [close 0
      [box 0
      [constant "b"
      [assign-local 0
      [refer-local 0
      [indirect
      [return]]]]]]
      [apply]
   -1]]]]]
[halt]]
```
