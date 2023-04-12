// Library (scheme repl)
import { Library } from "../system/expander/library.js"
import { List } from "../system/pair.js"
import { Sym } from "../system/symbol.js"

const libSchemeRepl = Library.create(List(Sym('scheme'), Sym('repl')));
const exports = [
    "interaction-environment",
];
for (const name of exports) {
    const s = Sym(name);
    libSchemeRepl.addExport(s, s);
}

export { libSchemeRepl };