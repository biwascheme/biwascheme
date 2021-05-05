# Conformance

## R7RS

(see [doc/r7rs](/doc/r7rs.html))

## R6RS

Since R6RS was the latest spec when the development of BiwaScheme is started (2007), BiwaScheme has some R6RS specific features, but not all of them. Currently there is no plan to complete them because conformance to R7RS-small has higher priority.

### R6RS Base library

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
  * no
* 11.15 Control features
  * ok
* 11.16 Iteration
  * ok
* 11.17 Quasiquotation
  * ok
* 11.18 Binding constructs for syntactic keywords
  * no
* 11.19 Macro transformers
  * no
* 11.20 Tail calls and tail contexts
  * ok

### R6RS Standard Libraries
      
* 1 Unicode
  * no
* 2 Bytevectors
  * no
* 3 List utilities
  * ok
* 4 Sorting
  * partially ok
* 5 Control structures
  * partially ok (except case-lambda)
* 6 Records
  * ok
* 7 Exceptions and conditions
  * no
* 8 I/O
  * partially ok
* 9 File system
  * ok (on Node.js)
* 10 Command-line access and exit values
  * ok (on Node.js)
* 11 Arithmetic
  * no
* 12 syntax-case
  * no
* 13 Hashtables
  * ok
* 14 Enumerations
  * ok
* 15 Composite library
  * no
* 16 eval
  * almost ok (eval)
* 17 Mutable pairs
  * no
* 18 Mutable strings
  * no
* 19 R5RS compatibility
  * no

## SRFIs

(Moved to [doc/features](/doc/features.html))
