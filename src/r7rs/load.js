// Library (scheme load)
import { Library } from "../system/expander/library.js"
import { List } from "../system/pair.js"
import { Sym } from "../system/symbol.js"

const libSchemeLoad = Library.create(List(Sym('scheme'), Sym('load')));
const exports = [
    "load",
];
for (const name of exports) {
    const s = Sym(name);
    libSchemeLoad.addExport(s, s);
}

export { libSchemeLoad };
