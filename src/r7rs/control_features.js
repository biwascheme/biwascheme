import { nil } from "../header.js";
import { Sym } from "../system/symbol.js"
import { Pair, List, isPair, isList, array_to_list, Cons } from "../system/pair.js"
import { isSymbol } from "../system/_types.js"
import { makeErMacroTransformer } from "../system/expander/macro_transformer.js"
// TODO
// procedure?
// apply
// set!
// unless
// when 
// quote
// quasiquote
// unquote
// unquote-splicing 
// let
// let*
// letrec
// letrec*
// do
// cond
// cond-expand 
// case
// case-lambda
// begin
// define 
// make-parameter 
// parameterize
// lambda
// features

const installControlFeatures = (lib) => { 
  lib.exportMacro(Sym("let"), makeErMacroTransformer(([form, rename, compare]) => {
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
};

export { installControlFeatures };
