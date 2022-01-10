// Acknowledgement: https://github.com/nyuichi/r7expander
import { nil, CoreEnv } from "../header.js";
import { isSymbol, isVector } from "./_types.js"
import { to_write } from "./_writer.js"
import { BiwaError, Bug } from "./error.js"
import { List, array_to_list, Cons, isPair, isList } from "./pair.js"
import { Sym } from "./symbol.js"
import { Environment } from "./expander/environment.js"
import { SyntacticClosure, isSyntacticClosure } from "./expander/syntactic_closure.js"
import { Library, isLibrary } from "./expander/library.js"
import { Macro } from "./expander/macro.js"
import { makeErMacroTransformer } from "./expander/macro_transformer.js"

const _BsCore = List(Sym('biwascheme'), Sym('core'));
const libBsCore = Library.create(_BsCore);

libBsCore.environment.installToplevelBinding(Sym("cons"), CoreEnv["cons"]);
libBsCore.export(Sym("cons"));

const letExpander = new Macro("let", libBsCore.environment, async ([form, xp]) => {
  return form;
});
libBsCore.environment.installExpander(Sym("let"), letExpander);
libBsCore.export(Sym("let"));

const ifExpander = new Macro("if", libBsCore.environment, async ([form, xp]) => {
  const l = form.to_array();
  switch (l.length) {
    case 3:
      return List(Sym("if"), await xp.expand(l[1]), await xp.expand(l[2]));
    case 4:
      return List(Sym("if"), await xp.expand(l[1]), await xp.expand(l[2]), await xp.expand(l[3]));
    default:
      throw new BiwaError("malformed if", form);
  }
});
libBsCore.environment.installExpander(Sym("if"), ifExpander);
libBsCore.export(Sym("if"));

const myOrExpander = new Macro("my-or", libBsCore.environment, makeErMacroTransformer(([form, rename, compare]) => {
  const [_, x, y] = form.to_array();
  // TODO: impl. `let`
  //return List(rename([Sym("let")]), List(List(rename([Sym("tmp")]), x)),
  //  List(rename([Sym("if")]), rename([Sym("tmp")]), rename([Sym("tmp")]), y));
}));
libBsCore.environment.installExpander(Sym("my-or"), myOrExpander);
libBsCore.export(Sym("my-or"));

export { libBsCore };
