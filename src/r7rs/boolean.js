import { Sym } from "../system/symbol.js"
import { makeErMacroTransformer } from "../system/expander/macro_transformer.js"
// boolean=?  boolean?  
// not
// or and

const installBoolean = (lib) => { 
  lib.exportMacro(Sym("or"), makeErMacroTransformer(([form, rename, compare]) => {
    const [_, x, y] = form.to_array();
    // TODO: impl. `let`
    //return List(rename([Sym("let")]), List(List(rename([Sym("tmp")]), x)),
    //  List(rename([Sym("if")]), rename([Sym("tmp")]), rename([Sym("tmp")]), y));
  }));
};

export { installBoolean };
