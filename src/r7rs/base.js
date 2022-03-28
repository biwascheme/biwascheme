import { Library, isLibrary } from "../system/expander/library.js"
import { List, Cons, isPair, isList } from "../system/pair.js"
import { Sym } from "../system/symbol.js"
import { installCore } from "../system/expander/core.js"
import { installBoolean } from "./boolean.js"
import { installControlFeatures } from "./control_features.js"

const libSchemeBase = Library.create(List(Sym('scheme'), Sym('base')));
installCore(libSchemeBase);
installBoolean(libSchemeBase);
installControlFeatures(libSchemeBase);

export { libSchemeBase };

// TODO
//equal?  eq?  eqv? 
