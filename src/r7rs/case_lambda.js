// Library (scheme case-lambda)
import { Library, isLibrary } from "../system/expander/library.js"
import { List, Cons, isPair, isList } from "../system/pair.js"
import { Sym } from "../system/symbol.js"

const libSchemeCaseLambda = Library.create(List(Sym('scheme'), Sym('case-lambda')));

// TODO: syntax case-lambda

export { libSchemeCaseLambda };

