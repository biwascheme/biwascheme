// Library (scheme char)
import { Library } from "../system/expander/library.js"
import { List } from "../system/pair.js"
import { Sym } from "../system/symbol.js"

const libSchemeChar = Library.create(List(Sym('scheme'), Sym('char')));
const exports = [
    // TODO: "char-alphabetic?", "char-ci<=?", "char-ci<?", "char-ci=?", "char-ci>=?", "char-ci>?", "char-downcase", "char-foldcase", "char-lower-case?", "char-numeric?", "char-upcase", "char-upper-case?", "char-whitespace?", "digit-value",
    "string-ci<=?", "string-ci<?",
    "string-ci=?", "string-ci>=?",
    "string-ci>?", "string-downcase", "string-upcase",
    //TODO: "string-foldcase",
];
for (const name of exports) {
    const s = Sym(name);
    libSchemeChar.addExport(s, s);
}

export { libSchemeChar };