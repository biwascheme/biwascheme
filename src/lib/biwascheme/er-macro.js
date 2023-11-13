// Library (biwascheme er-macro)
import { Library } from "../../system/expander/library.js"
import { List } from "../../system/pair.js"
import { Sym } from "../../system/symbol.js"

const libBiwaschemeErMacro = Library.create(List(Sym('biwascheme'), Sym('er-macro')));
const exports = [
     "er-macro-transformer"
];
for (const name of exports) {
    const s = Sym(name);
    libBiwaschemeErMacro.addExport(s, s);
}

export { libBiwaschemeErMacro };