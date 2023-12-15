// Library (biwascheme er-macro)
import { Library } from "../../system/expander/library.js"
import { List } from "../../system/pair.js"
import { Sym } from "../../system/symbol.js"
import { define_libfunc, alias_libfunc, define_syntax, define_scmfunc,
         assert_number, assert_integer, assert_real, assert_between, assert_string,
         assert_char, assert_symbol, assert_port, assert_pair, assert_list,
         assert_function, assert_closure, assert_procedure, assert_date, assert, deprecate } from "../../library/infra.js"; 
import { ErMacroTransformer } from "../../system/expander/macro_transformer.js"

define_libfunc("er-macro-transformer", 1, 1, function(ar) {
  assert_procedure(ar[0]);
  return new ErMacroTransformer(ar[0]);
});

const libBiwaschemeErMacro = Library.create(List(Sym('biwascheme'), Sym('er-macro')));
const exports = [
     "er-macro-transformer"
];
for (const name of exports) {
    const s = Sym(name);
    libBiwaschemeErMacro.addExport(s, s);
}

export { libBiwaschemeErMacro };
