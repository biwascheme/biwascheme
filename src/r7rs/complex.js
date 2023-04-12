// Library (scheme complex)
import { Library } from "../system/expander/library.js"
import { List } from "../system/pair.js"
import { Sym } from "../system/symbol.js"

const libSchemeComplex = Library.create(List(Sym('scheme'), Sym('complex')));
const exports = [
    "angle", "magnitude", "make-rectangular", "make-polar", "real-part", "imag-part"
];
for (const name of exports) {
    const s = Sym(name);
    libSchemeComplex.addExport(s, s);
}

export { libSchemeComplex };