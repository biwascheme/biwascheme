import { Library, isLibrary } from "../system/expander/library.js"
import { List, Cons, isPair, isList } from "../system/pair.js"
import { Sym } from "../system/symbol.js"
import { libBsCore } from "../system/core.js"

const libSchemeBase = Library.create(List(Sym('scheme'), Sym('base')));
libSchemeBase.import(libBsCore);
libSchemeBase.export(Sym("if"));
libSchemeBase.export(Sym("my-or"));

export { libSchemeBase };

// TODO
//define-record-type 
//equal?  eq?  eqv? 
//include-ci 
//include
