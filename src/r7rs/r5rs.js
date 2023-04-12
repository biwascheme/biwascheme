// Library (scheme r5rs)
import { Library } from "../system/expander/library.js"
import { List } from "../system/pair.js"
import { Sym } from "../system/symbol.js"

const libSchemeR5RS = Library.create(List(Sym('scheme'), Sym('r5rs')));
const exports = [
    // TODO: export these names except those unimplemented.
    // * +
    // - /
    // < <=
    // = >
    // >= abs
    // acos and
    // angle append
    // apply asin
    // assoc assq
    // assv atan
    // begin boolean?
    // caaaar caaadr
    // caaar caadar
    // caaddr caadr
    // caar cadaar
    // cadadr cadar
    // caddar cadddr
    // caddr cadr
    // call-with-current-continuation
    // call-with-input-file call-with-output-file
    // call-with-values car
    // case cdaaar
    // cdaadr cdaar
    // cdadar cdaddr
    // cdadr cdar
    // cddaar cddadr
    // cddar cdddar
    // cddddr cdddr
    // cddr cdr
    // ceiling char->integer
    // char-alphabetic? char-ci<=?
    // char-ci<? char-ci=?
    // char-ci>=? char-ci>?
    // char-downcase char-lower-case?
    // char-numeric? char-ready?
    // char-upcase char-upper-case?
    // char-whitespace? char<=?
    // char<? char=?
    // char>=? char>?
    // char? close-input-port
    // close-output-port complex?
    // cond cons
    // cos current-input-port
    // current-output-port define
    // define-syntax delay
    // denominator display
    // do dynamic-wind
    // eof-object? eq?
    // equal? eqv?
    // eval even?
    // exact->inexact exact?
    // exp expt
    // floor for-each
    // force gcd
    // if imag-part
    // inexact->exact inexact?
    // input-port? integer->char
    // integer? interaction-environment
    // lambda lcm
    // length let
    // let* let-syntax
    // letrec letrec-syntax
    // list list->string
    // list->vector list-ref
    // list-tail list?
    // load log
    // magnitude make-polar
    // make-rectangular make-string
    // make-vector map
    // max member
    // memq memv
    // min modulo
    // negative? newline
    // not null-environment
    // null? number->string
    // number? numerator
    // odd? open-input-file
    // open-output-file or
    // output-port? pair?
    // peek-char positive?
    // procedure? quasiquote
    // quote quotient
    // rational? rationalize
    // read read-char
    // real-part real?
    // remainder reverse
    // round
    // scheme-report-environment
    // set! set-car!
    // set-cdr! sin
    // sqrt string
    // string->list string->number
    // string->symbol string-append
    // string-ci<=? string-ci<?
    // string-ci=? string-ci>=?
    // string-ci>? string-copy
    // string-fill! string-length
    // string-ref string-set!
    // string<=? string<?
    // string=? string>=?
    // string>? string?
    // substring symbol->string
    // symbol? tan
    // truncate values
    // vector vector->list
    // vector-fill! vector-length
    // vector-ref vector-set!
    // vector? with-input-from-file
    // with-output-to-file write
    // write-char zero?
];
for (const name in exports) {
    const s = Sym(name);
    libSchemeR5RS.addExport(s, s);
}

export { libSchemeR5RS };
