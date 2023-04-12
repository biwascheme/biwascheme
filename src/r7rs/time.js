// Library (scheme time)
import { Library } from "../system/expander/library.js"
import { List } from "../system/pair.js"
import { Sym } from "../system/symbol.js"

const libSchemeTime = Library.create(List(Sym('scheme'), Sym('time')));
const exports = [
    //TODO "current-jiffy current-second jiffies-per-second",
];
for (const name of exports) {
    const s = Sym(name);
    libSchemeTime.addExport(s, s);
}

export { libSchemeTime };