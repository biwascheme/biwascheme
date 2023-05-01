// Library (scheme base)
import { nil, undef } from "../header.js";
import { Library, isLibrary } from "../system/expander/library.js"
import { isSymbol } from "../system/_types.js"
import { inspect } from "../system/_writer.js"
import { BiwaError } from "../system/error.js"
import { List, Cons, Pair, isPair, isList } from "../system/pair.js"
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

libSchemeBase.exportMacro(Sym("and"), makeErMacroTransformer(([x, rename, compare]) => {
  // (and a b c) => (if a (if b c #f) #f)
  //todo: check improper list
  if (x.cdr === nil) return true;

  var objs = x.cdr.to_array();
  var i = objs.length-1;
  var t = objs[i];
  for(i=i-1; i>=0; i--)
    t = List(rename(Sym("if")), objs[i], t, false);

  return t;
}));

libSchemeBase.exportMacro(Sym("or"), makeErMacroTransformer(([x, rename, compare]) => {
  // (or a b c) => (if a a (if b b (if c c #f)))
  //todo: check improper list

  var objs = x.cdr.to_array()
  var f = false;
  for(let i=objs.length-1; i>=0; i--)
    f = List(rename(Sym("if")), objs[i], objs[i], f);

  return f;
}));

libSchemeBase.exportMacro(Sym("when"), makeErMacroTransformer(([x, rename, compare]) => {
  //(when test body ...)
  //=> (if test (begin body ...) #<undef>)
  const test = x.cdr.car, body = x.cdr.cdr;

  return new Pair(rename(Sym("if")),
           new Pair(test,
             new Pair(new Pair(rename(Sym("begin")), body),
               new Pair(undef, nil))));
}));

libSchemeBase.exportMacro(Sym("unless"), makeErMacroTransformer(([x, rename, compare]) => {
  //(unless test body ...)
  //=> (if (not test) (begin body ...) #<undef>)
  const test = x.cdr.car, body = x.cdr.cdr;

  return new Pair(rename(Sym("if")),
           new Pair(new Pair(rename(Sym("not")), new Pair(test, nil)),
             new Pair(new Pair(rename(Sym("begin")), body),
               new Pair(undef, nil))));
}));

// TODO cond-expand

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

libSchemeBase.exportMacro(Sym("let*"), makeErMacroTransformer(([x, rename, compare]) => {
  //(let* ((a 1) (b a)) (print a) (+ a b))
  //-> (let ((a 1))
  //     (let ((b a)) (print a) (+ a b)))
  const binds = x.cdr.car, body = x.cdr.cdr;

  if(binds === nil)
    return new Pair(rename(Sym("let")), new Pair(nil, body));

  if(!(binds instanceof Pair))
    throw new BiwaError("let*: need a pair for bindings: got "+to_write(binds));

  var ret = null;
  binds.to_array().reverse().forEach(function(bind){
    ret = new Pair(rename(Sym("let")),
             new Pair(new Pair(bind, nil),
               ret == null ? body : new Pair(ret, nil)));
  })
  return ret;
}));

const letRecStarTransfomer = makeErMacroTransformer(([x, rename, compare]) => {
  const binds = x.cdr.car, body = x.cdr.cdr;

  if(!(binds instanceof Pair))
    throw new BiwaError("letrec*: need a pair for bindings: got "+to_write(binds));

  var ret = body;
  binds.to_array().reverse().forEach(function(bind){
    ret = new Pair(new Pair(rename(Sym("set!")), bind),
            ret);
  })
  var letbody = nil;
  binds.to_array().reverse().forEach(function(bind){
    letbody = new Pair(new Pair(bind.car,
                         new Pair(undef, nil)),
                letbody);
  })
  return new Pair(rename(Sym("let")),
           new Pair(letbody,
             ret));
});
libSchemeBase.exportMacro(Sym("letrec"), letRecStarTransfomer); 
libSchemeBase.exportMacro(Sym("letrec*"), letRecStarTransfomer); 

libSchemeBase.exportMacro(Sym("let-values"), makeErMacroTransformer(([x, rename, compare]) => {
  // (let-values (((a b) (values 1 2))
  //               ((c d . e) (values 3 4 a)))
  //              (print a b c d e))
  // =>
  // (let ((#<gensym1> (lambda () (values 1 2)))
  //       (#<gensym2> (lambda () (values 3 4 a))))
  //   (let*-values (((a b) #<gensym1>)
  //                 ((c d . e) #<gensym2>))
  //                 (print a b c d e)))
  var mv_bindings = x.cdr.car;
  var body = x.cdr.cdr;
  var ret = null;

  var let_bindings = nil;
  var let_star_values_bindings = nil;
  mv_bindings.to_array().reverse().forEach(function (item) {
  var init = item.cdr.car;
  var tmpsym = gensym()
  var binding = new Pair(tmpsym,
       new Pair(
          new Pair(rename(Sym("lambda")), new Pair(nil,
                   new Pair(init, nil))),
          nil));
  let_bindings = new Pair(binding, let_bindings);

  var formals = item.car;
  let_star_values_bindings = new Pair(new Pair (formals, new Pair(new Pair(tmpsym, nil), nil)),
              let_star_values_bindings);
    });

    var let_star_values = new Pair(rename(Sym("let*-values")),
           new Pair(let_star_values_bindings,
              body));
    ret = new Pair(rename(Sym("let")),
       new Pair(let_bindings,
          new Pair (let_star_values, nil)));
    return ret;

}));

libSchemeBase.exportMacro(Sym("let*-values"), makeErMacroTransformer(([x, rename, compare]) => {
  // (let*-values (((a b) (values 1 2))
  //               ((c d . e) (values 3 4 a)))
  //   (print a b c d e))
  // -> (call-with-values
  //      (lambda () (values 1 2))
  //      (lambda (a b)
  //        (call-with-values
  //          (lambda () (values 3 4 a))
  //          (lambda (c d . e)
  //            (print a b c d e)))))
  var mv_bindings = x.cdr.car;
  var body = x.cdr.cdr;

  var ret = null;

  mv_bindings.to_array().reverse().forEach(function(item){
    var formals = item.car, init = item.cdr.car;
    ret = new Pair(rename(Sym("call-with-values")),
            new Pair(new Pair(rename(Sym("lambda")),
                       new Pair(nil,
                         new Pair(init, nil))),
              new Pair(new Pair(rename(Sym("lambda")),
                         new Pair(formals,
                           (ret == null ? body
                                        : new Pair(ret, nil)))), nil)));
  });
  return ret;
}));

libSchemeBase.exportMacro(Sym("do"), makeErMacroTransformer(([x, rename, compare]) => {
  //(do ((var1 init1 step1)
  //     (var2 init2 step2) ...)
  //    (test expr1 expr2 ...)
  //  body1 body2 ...)
  //=> (let loop` ((var1 init1) (var2 init2) ...)
  //     (if test
  //       (begin expr1 expr2 ...)
  //       (begin body1 body2 ...
  //              (loop` step1 step2 ...)))))

  // parse arguments
  if(!isPair(x.cdr))
    throw new BiwaError("do: no variables of do");
  var varsc = x.cdr.car;
  if(!isPair(varsc))
    throw new BiwaError("do: variables must be given as a list");
  if(!isPair(x.cdr.cdr))
    throw new BiwaError("do: no resulting form of do");
  var resultc = x.cdr.cdr.car;
  var bodyc = x.cdr.cdr.cdr;

  // construct subforms
  var loop = gensym();

  var init_vars = array_to_list(varsc.map(function(var_def){
    var a = var_def.to_array();
    return List(a[0], a[1]);
  }));

  var test = resultc.car;
  var result_exprs = new Pair(rename(Sym("begin")), resultc.cdr);

  var next_loop = new Pair(loop, array_to_list(varsc.map(function(var_def){
    var a = var_def.to_array();
    return a[2] || a[0];
  })));
  var body_exprs = new Pair(rename(Sym("begin")), bodyc).concat(List(next_loop));

  // combine subforms
  return List(rename(Sym("let")),
              loop,
              init_vars,
              List(rename(Sym("if")),
                   test,
                   result_exprs,
                   body_exprs));
}));

// TODO guard

libSchemeBase.exportMacro(Sym("quasiquote"), makeErMacroTransformer(async ([x, rename, compare]) => {
  const expand_qq = async function(f, lv){
    if(isSymbol(f) || f === nil){
      return List(rename(Sym("quote")), f);
    }
    else if(f instanceof Pair){
      var car = f.car;
      if(car instanceof Pair && await compare(car.car, Sym("unquote-splicing"))){
        if(lv == 1) {
          if (f.cdr === nil) {
            return f.car.cdr.car;
          } else {
            return List(rename(Sym("append")),
                        f.car.cdr.car,
                        await expand_qq(f.cdr, lv));
          }
        }
        else
          return List(rename(Sym("cons")),
                      List(rename(Sym("list")),
                          List(rename(Sym("quote")), rename(Sym("unquote-splicing"))),
                          await expand_qq(f.car.cdr.car, lv-1)),
                      await expand_qq(f.cdr, lv));
      }
      else if(await compare(car, Sym("unquote"))){
        if(lv == 1)
          return f.cdr.car;
        else
          return List(rename(Sym("list")),
                      List(rename(Sym("quote")), rename(Sym("unquote"))),
                      await expand_qq(f.cdr.car, lv-1));
      }
      else if(await compare(car, Sym("quasiquote")))
        return List(rename(Sym("list")),
                    List(rename(Sym("quote")), rename(Sym("quasiquote"))),
                    await expand_qq(f.cdr.car, lv+1));
      else
        return List(rename(Sym("cons")),
                    await expand_qq(f.car, lv),
                    await expand_qq(f.cdr, lv));
    }
    else if(f instanceof Array){
      var vecs = [[]];
      for(var i=0; i<f.length; i++){
        if(f[i] instanceof Pair && await compare(f[i].car, Sym("unquote-splicing"))) {
          if (lv == 1) {
            var item = List(rename(Sym("list->vector")), f[i].cdr.car);
            item["splicing"] = true;
            vecs.push(item);
            vecs.push([]);
          }
          else {
            var item = List(rename(Sym("cons")),
                        List(rename(Sym("list")),
                              List(rename(Sym("quote")), rename(Sym("unquote-splicing"))),
                              await expand_qq(f[i].car.cdr.car, lv-1)),
                        await expand_qq(f[i].cdr, lv));
            vecs[vecs.length-1].push(item);
          }
        }
        else {
          // Expand other things as the same as if they are in a list quasiquote
          vecs[vecs.length-1].push(await expand_qq(f[i], lv));
        }
      }

      var vectors = vecs.map(function(vec){
        if (vec["splicing"]) {
          return vec;
        }
        else {
          return Cons(rename(Sym("vector")),
                      array_to_list(vec));
        }
      });
      if (vectors.length == 1) {
        return Cons(rename(Sym("vector")),
                    array_to_list(vecs[0]));
      }
      else {
        return Cons(rename(Sym("vector-append")),
                    array_to_list(vectors));
      }
    }
    else
      return f;
  };
  return expand_qq(x.cdr.car, 1);
}))
libSchemeBase.exportMacro(Sym("unquote"), makeErMacroTransformer(([x, rename, compare]) => {
  throw new BiwaError("unquote(,) must be inside quasiquote(`)");
}))
libSchemeBase.exportMacro(Sym("unquote-splicing"), makeErMacroTransformer(([x, rename, compare]) => {
  throw new BiwaError("unquote-splicing(,@) must be inside quasiquote(`)");
}))

// Note: parameterize is defined in expander/core.js

// 4.3 Macros
// Note: define-syntax, syntax-error, let-syntax and letrec-syntax are defined in expander/core.js
// TODO: syntax-rules(...) 

// 5 Program structure
// TODO: define-values
// Note: include include-ci define-record-type are defined in expander/core.js

export { libSchemeBase };