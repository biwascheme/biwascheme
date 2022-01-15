import { Sym } from "../system/symbol.js"
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
    const [_, x, y] = form.to_array();
    // TODO: impl. `let`
    //return List(rename([Sym("let")]), List(List(rename([Sym("tmp")]), x)),
    //  List(rename([Sym("if")]), rename([Sym("tmp")]), rename([Sym("tmp")]), y));
  }));
};

export { installControlFeatures };
