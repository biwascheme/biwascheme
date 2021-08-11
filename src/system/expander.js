// Acknowledgement: https://github.com/nyuichi/r7expander
import { nil } from "../header.js";
import { isSymbol, isVector } from "./_types.js"
import { to_write } from "./_writer.js"
import { BiwaError, Bug } from "./error.js"
import { List, array_to_list, Cons, isPair, isList } from "./pair.js"
import { Sym } from "./symbol.js"
import { Environment } from "./expander/environment.js"
import { SyntacticClosure, isSyntacticClosure } from "./expander/syntactic_closure.js"
import { Library, isLibrary } from "./expander/library.js"
import { Macro } from "./expander/macro.js"

const libBsCore = List(Sym('biwascheme'), Sym('core'));
Library.makeLibrary(libBsCore);
Library.withLibrary(libBsCore, () => {
  const topEnv = Environment.currentToplevelEnvironment;
  const ifExpander = new Macro("if", topEnv, (form, env) => {
    const l = form.to_array();
    switch (l.length) {
      case 3:
        return List(Sym("if"), env.expand(l[1]), env.expand(l[2]));
      case 4:
        return List(Sym("if"), env.expand(l[1]), env.expand(l[2]), env.expand(l[3]));
      default:
        throw new BiwaError("malformed if", form);
    }
  });
  topEnv.installExpander(Sym("if"), ifExpander);
  Library.libraryExport(Sym("if"));
});

const libSchemeBase = List(Sym('scheme'), Sym('base'));
Library.makeLibrary(libSchemeBase);
Library.withLibrary(libSchemeBase, () => {
  Library.libraryImport(libBsCore);
  Library.libraryExport(Sym("if"));
});

export { Environment, Library };
