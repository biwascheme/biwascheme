// Library (scheme base)
import { Library, isLibrary } from "../system/expander/library.js"
import { List, Cons, isPair, isList } from "../system/pair.js"
import { Sym } from "../system/symbol.js"
import { makeErMacroTransformer } from "../system/expander/macro_transformer.js"
import { installCore } from "../system/expander/core.js"

const libSchemeBase = Library.create(List(Sym('scheme'), Sym('base')));
installCore(libSchemeBase);

const exports = [
    // 4.2 Derived expression types
    // TODO: make-parameter

    // 6.1 Equivalence predicates
    "eqv?", "eq?", "equal?",

    // 6.2 Numbers
    "number?", "complex?", "real?", "rational?", "integer?",
    //TODO: exact? inexact?  exact exact-integer? inexact
    "*", "+", "-", "/",
    "<", "<=", "=", ">", ">=",
    "zero?", "positive?", "negative?", "odd?", "even?",
    "max", "min", "abs",
    "floor", "floor/", "floor-quotient", "floor-remainder",
    "truncate", "truncate/", "truncate-quotient", "truncate-remainder",
    "ceiling", "round",
    "denominator", "numerator",
    "quotient", "remainder", "modulo",
    // TODO: "gcd", "lcm", "rationalize", "square",
    "exact-integer-sqrt",  "expt",
    "number->string", "string->number",

    // 6.3 Boolean
    "boolean=?", "boolean?", "not",

    // 6.4 Pairs and lists
    "pair?", "cons", "car", "cdr", "set-car!", "set-cdr!",
    "caar", "cadr", "cdar", "cddr",
    "null?", "list?", "make-list", "list", "length", 
    "append", "reverse", "list-tail", "list-ref", "list-set!", "list-copy",
    "memq", "memv", "member", "assq", "assv", "assoc",

    // 6.5 Symbols
    "symbol?", "symbol=?", "symbol->string", "string->symbol",

    // 6.6 Characters
    "char<?", "char>=?", "char?", "char<=?", "char=?", "char>?",
    "integer->char", "char->integer",

    // 6.7 Strings
    "string?", "make-string", "string", "string-length",
    "string-ref", "string-set!",
    "string=?", "string<=?", "string<?", "string>=?", "string>?",
    "substring", "string-append", "string->list", "list->string",
    "string-copy", "string-copy!", "string-fill!",

    // 6.8 Vectors
    "vector?", "make-vector", "vector", "vector-length",
    "vector-ref", "vector-set!", "vector->list", "list->vector",
    "vector->string", "string->vector", "vector-copy", "vector-copy!",
    "vector-append", "vector-fill!",

    // 6.9 Bytevectors
    // TODO: bytevector bytevector-append bytevector-copy bytevector-copy! bytevector-length bytevector-u8-ref bytevector-u8-set!  bytevector?  string->utf8 utf8->string make-bytevector

    // 6.10 Control features
    "procedure?", "apply", "map", "string-map", "vector-map", "for-each",
    "string-for-each", "vector-for-each",
    "call-with-current-continuation",
    //"call/cc" is core syntax
    "values", "call-with-values", "dynamic-wind",

    // 6.11 Exceptions
    // TODO: with-exception-handler raise raise-continuable error error-object-message error-object?  error-object-irritants file-error?read-error? 

    // 6.13 Input and output
    "call-with-port", "port?", "textual-port?", "binary-port?",
    "input-port?", "output-port?",
    //TODO: "input-port-open?", "output-port-open?",
    "current-error-port", "current-input-port", "current-output-port",
    "close-input-port", "close-output-port", "close-port",
    //TODO: "open-input-string", "open-output-string", "get-output-string",
    //TODO: "open-input-bytevector", "open-output-bytevector", "get-output-bytevector",

    "read-char", "peek-char", "char-ready?",
    "read-u8", "peek-u8", "u8-ready?",
    "read-string", "read-line", "eof-object", "eof-object?",
    // TODO: read-bytevector read-bytevector! 

    "newline", "write-char", "write-string", "write-u8",
    //TODO: "write-bytevector", "flush-output-port ""

    // 6.14 System interface
    //TODO: features 
];
for (const name of exports) {
    const s = Sym(name);
    libSchemeBase.addExport(s, s);
}

// 4.2 Derived expression types
// TODO cond(else, =>) case and 

libSchemeBase.exportMacro(Sym("or"), makeErMacroTransformer(([form, rename, compare]) => {
    const [_, x, y] = form.to_array();
    return List(rename([Sym("let")]), List(List(rename([Sym("tmp")]), x)),
            List(rename([Sym("if")]), rename([Sym("tmp")]), rename([Sym("tmp")]), y));
}));

// TODO when unless cond-expand

libSchemeBase.exportMacro(Sym("let"), makeErMacroTransformer(([form, rename, compare]) => {
  //(let ((a 1) (b 2)) (print a) (+ a b))
  //=> ((lambda (a b) (print a) (+ a b)) 1 2)
  let x = form;
  let name = null;
  if (isSymbol(x.cdr.car)) {
    name = x.cdr.car;
    x = x.cdr;
  }
  const binds = x.cdr.car, body = x.cdr.cdr;

  if((!isPair(binds)) && binds != nil){
    throw new BiwaError("let: need a pair for bindings: got "+to_write(binds));
  }

  let vars = nil, vals = nil;
  for(let p=binds; isPair(p); p=p.cdr){
    if(!isPair(p.car)){
      throw new BiwaError("let: need a pair for bindings: got "+to_write(p.car));
    }
    vars = new Pair(p.car.car, vars);
    vals = new Pair(p.car.cdr.car, vals);
  }

  let lambda = null;
  if (name) {
    // (let loop ((a 1) (b 2)) body ..)
    //=> (letrec ((loop (lambda (a b) body ..))) (loop 1 2))
    vars = array_to_list(vars.to_array().reverse());
    vals = array_to_list(vals.to_array().reverse());

    const body_lambda = new Pair(Sym("lambda"), new Pair(vars, body));
    const init_call = new Pair(name, vals);

    lambda = List(Sym("letrec"),
                  new Pair(List(name, body_lambda), nil),
                  init_call);
  }
  else {
    lambda = new Pair(new Pair(Sym("lambda"),
                               new Pair(vars, body)),
                      vals);
  }
  return lambda;
}));

// TODO let* let*-values let-values letrec letrec*
// TODO do
// TODO guard
// TODO quasiquote unquote unquote-splicing
// Note: parameterize is defined in expander/core.js

// 4.3 Macros
// Note: define-syntax, syntax-error, let-syntax and letrec-syntax are defined in expander/core.js
// TODO: syntax-rules(...) 

// 5 Program structure
// TODO: define-values
// Note: include include-ci define-record-type are defined in expander/core.js

export { libSchemeBase };