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

const _BsCore = List(Sym('biwascheme'), Sym('core'));
const libBsCore = Library.create(_BsCore);

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

const _SchemeBase = List(Sym('scheme'), Sym('base'));
const libSchemeBase = Library.create(_SchemeBase);
libSchemeBase.import(libBsCore);
libSchemeBase.export(Sym("if"));

const mangle = (spec) => spec.to_write;
class Libraries {
  constructor() {
    this.libraries = new Map();
  }
  
  get(spec) {
    const key = mangle(spec)
    if (this.libraries.has(key)) {
      return this.libraries.get(key);
    } else {
      throw new BiwaError("unknown library", spec);
    }
  }

  set(spec, lib) {
    this.libraries.set(mangle(spec), lib);
  }
}

const coreLibraries = new Libraries();
coreLibraries.set(_SchemeBase, libSchemeBase); 

export { coreLibraries };
