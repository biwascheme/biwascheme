// Acknowledgement: https://github.com/nyuichi/r7expander
import { nil } from "../header.js";
import { isSymbol, isPair, isList, isVector } from "./_types.js"
import { to_write } from "./_writer.js"
import { BiwaError, Bug } from "./error.js"
import { List, array_to_list, Cons } from "./pair.js"
import { Sym } from "./symbol.js"
import { Environment } from "./expander/environment.js"
import { SyntacticClosure, isSyntacticClosure } from "./expander/syntactic_closure.js"
import { Library, isLibrary } from "./expander/library.js"
import { Expander } from "./expander/expander.js"

const identifierEquals = (id1, env1, id2, env2) => {
  TODO
};

const libBsCore = List(Sym('biwascheme'), Sym('core'));
Library.makeLibrary(libBsCore);
Library.withLibrary(libBsCore, () => {
  const env = Environment.currentToplevelEnvironment;
  const ifExpander = new Expander((form, env) => {
    const l = form.to_array();
    switch (l.length) {
      case 3:
        return List(Sym("if"), env.expand(l[1]), env.expand(l[2]));
      case 4:
        return List(Sym("if"), env.expand(l[1]), env.expand(l[2]), env.expand(l[3]));
      default:
        throw new BiwaError("ifExpander: malformed if", form);
    }
  }, env);
  env.installExpander(Sym("if"), ifExpander);
  Library.libraryExport(Sym("if"));
});

const libSchemeBase = List(Sym('scheme'), Sym('base'));
Library.makeLibrary(libSchemeBase);
Library.withLibrary(libSchemeBase, () => {
  Library.libraryImport(libBsCore);
  Library.libraryExport(Sym("if"));
});

export { Environment, Library };
