// Library (scheme read)
import { Library } from "../system/expander/library.js"
import { List } from "../system/pair.js"
import { Sym } from "../system/symbol.js"

const libSchemeRead = Library.create(List(Sym('scheme'), Sym('read')));
const exports = [
    "read",
];
for (const name of exports) {
    const s = Sym(name);
    libSchemeRead.addExport(s, s);
}

export { libSchemeRead };