// How to run
//   $ node ./src/new_expander_dev.js
// See https://github.com/biwascheme/biwascheme/pull/192#issuecomment-673534970
// if it doesn't work
import { to_write } from "./system/_writer.js";
import Call from "./system/call.js";
import Parser from "./system/parser.js";
import { array_to_list } from "./system/pair.js";
import { Library } from "./system/expander/library.js"
import { Engine } from "./system/engine.js";

const engine = new Engine();

const forms = array_to_list(Parser.parse(`
  (import (scheme base))
  (if a b c)
  (print 1)
`));

//let x = Library.expandProgram(forms);
//while (x instanceof Call) {
//  x = x.after([x.proc(x.args)]);
//}
//console.log(to_write(x))
engine.expandToplevelProgram(forms)
  .then(x => console.log(to_write(x)))
  .catch(console.log)
