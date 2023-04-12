// Library (scheme write)
import { Library } from "../system/expander/library.js"
import { List } from "../system/pair.js"
import { Sym } from "../system/symbol.js"

const libSchemeWrite = Library.create(List(Sym('scheme'), Sym('write')));
const exports = [
    "display", "write", "write-shared", "write-simple",
];
for (const name of exports) {
    const s = Sym(name);
    libSchemeWrite.addExport(s, s);
}

export { libSchemeWrite };