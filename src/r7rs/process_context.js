// Library (scheme process-context)
import { Library } from "../system/expander/library.js"
import { List } from "../system/pair.js"
import { Sym } from "../system/symbol.js"

const libSchemeProcessContext = Library.create(List(Sym('scheme'), Sym('process-context')));
const exports = [
    "command-line", "exit", "get-environment-variable", "get-environment-variables",
    // TODO emergency-exit 
];
for (const name of exports) {
    const s = Sym(name);
    libSchemeProcessContext.addExport(s, s);
}

export { libSchemeProcessContext };
