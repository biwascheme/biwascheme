// Libray (scheme inexact)
import { Library } from "../system/expander/library.js"
import { List } from "../system/pair.js"
import { Sym } from "../system/symbol.js"

const libSchemeInexact = Library.create(List(Sym('scheme'), Sym('inexact')));
const exports = [
    "cos", "sin", "tan", "acos", "asin", "atan", 
    "sqrt", "log", "exp", "finite?", "infinite?", "nan?"
];
for (const name of exports) {
    const s = Sym(name);
    libSchemeInexact.addExport(s, s);
}

export { libSchemeInexact };