// Library (scheme lazy)
import { Library } from "../system/expander/library.js"
import { List } from "../system/pair.js"
import { Sym } from "../system/symbol.js"

const libSchemeLazy = Library.create(List(Sym('scheme'), Sym('lazy')));
const exports = [
    "delay", "delay-force", "force", "make-promise", "promise?",
];
for (const name of exports) {
    const s = Sym(name);
    libSchemeLazy.addExport(s, s);
}

export { libSchemeLazy };
