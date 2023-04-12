// Library (scheme eval)
import { Library } from "../system/expander/library.js"
import { List } from "../system/pair.js"
import { Sym } from "../system/symbol.js"

const libSchemeEval = Library.create(List(Sym('scheme'), Sym('eval')));
const exports = [
    "eval",
    //TODO "environment"
];
for (const name of exports) {
    const s = Sym(name);
    libSchemeEval.addExport(s, s);
}

export { libSchemeEval };