// Library (scheme file)
import { Library } from "../system/expander/library.js"
import { List } from "../system/pair.js"
import { Sym } from "../system/symbol.js"

const libSchemeFile = Library.create(List(Sym('scheme'), Sym('file')));
const exports = [
    // Note: these procedures are supported only on Node.js
    "delete-file", "file-exists?",
    // TODO open-binary-input-file open-binary-output-file open-input-file open-output-file with-input-from-file with-output-to-file
];

for (const name of exports) {
    const s = Sym(name);
    libSchemeFile.addExport(s, s);
}

export { libSchemeFile };
