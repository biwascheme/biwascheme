# Conformance

## Implementation Status

BiwaScheme has most features of [R6RS](http://www.r6rs.org/) Base library.
The biggest features not implemented are errors and `syntax-rules`
(you can use `define-macro` instead).

### R6RS Base library)

* 11.4 Expressions
  * ok
* 11.5 Equivalence predicates
  * ok
* 11.6 Procedure predicate
  * ok
* 11.7.4.1 Numerical operations
  * almost ok
* 11.8 Booleans
  * ok
* 11.9 Paris and lists
  * ok
* 11.10 Symbols
  * ok
* 11.11 Characters
  * ok
* 11.12 Strings
  * ok
* 11.13 Vectors
  * ok
* 11.14 Errors and violations
  * not yet
* 11.15 Control features
  * ok
* 11.16 Iteration
  * ok
* 11.17 Quasiquotation
  * almost ok (except vector quasiquotation))
* 11.18 Binding constructs for syntactic keywords
  * not yet
* 11.19 Macro transformers
  * not yet
* 11.20 Tail calls and tail contexts
  * ok

### R6RS Standard Libraries
      
* 1 Unicode
  * not yet
* 2 Bytevectors
  * no plan
* 3 List utilities
  * ok
* 4 Sorting
  * partially ok
* 5 Control structures
  * partially ok (except case-lambda)
* 6 Records
  * ok
* 7 Exceptions and conditions
  * not yet
* 8 I/O
  * partially ok
* 9 File system
  * ok (on Node.js)
* 10 Command-line access and exit values
  * ok (on Node.js)
* 11 Arithmetic
  * not yet
* 12 syntax-case
  * not yet
* 13 Hashtables
  * ok
* 14 Enumerations
  * ok
* 15 Composite library
  * not yet
* 16 eval
  * almost ok (eval)
* 17 Mutable pairs
  * no plan
* 18 Mutable strings
  * maybe? (JavaScript strings are immutable, so this will make string operations much slower)
* 19 R5RS compatibility
 * no plan

(no plan = paches are welcome)

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
* 30 multi-line comment
* 38 write/ss 
  * partially - `write/ss`
* 43 vector 
  * partially - `vector-append`
* 62 s-expr comment
* 98 get-environment-variable(s) 
  * Node.js only
