// How to run
//   $ node ./src/new_expander_dev.js
// See https://github.com/biwascheme/biwascheme/pull/192#issuecomment-673534970
// if it doesn't work
import BiwaScheme from "./main.js";

import { Environment, Library } from "./system/expander.js"

const forms = BiwaScheme.array_to_list(BiwaScheme.Parser.parse(`
  (import (scheme base))
  (if a b c)
`));
console.log(BiwaScheme.to_write(Library.expandProgram(forms)))
