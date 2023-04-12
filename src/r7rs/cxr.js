// Library (scheme cxr)
import { Library } from "../system/expander/library.js"
import { List } from "../system/pair.js"
import { Sym } from "../system/symbol.js"

const libSchemeCxr = Library.create(List(Sym('scheme'), Sym('cxr')));
const exports = [
    "caaaar", "caaadr", "caaar", "caadar", "caaddr", "caadr", "cadaar", "cadadr", "cadar", "caddar", "cadddr", "caddr", "cdaaar", "cdaadr", "cdaar", "cdadar", "cdaddr", "cdadr", "cddaar", "cddadr", "cddar", "cdddar", "cddddr", "cdddr"
];
for (const name of exports) {
    const s = Sym(name);
    libSchemeCxr.addExport(s, s);
}

export { libSchemeCxr };

