// Acknowledgement: https://github.com/nyuichi/r7expander
//
// How to run
//   $ node ./src/new_expander_dev.js
// See https://github.com/biwascheme/biwascheme/pull/192#issuecomment-673534970
// if it doesn't work
import { to_write } from "./system/_writer.js";
import Call from "./system/call.js";
import Parser from "./system/parser.js";
import { List, array_to_list } from "./system/pair.js";
import { Sym } from "./system/symbol.js";
import { Library } from "./system/expander/library.js"
import { Engine } from "./system/engine.js";

import "./library/r6rs_lib.js"

// Example 1
//const forms = array_to_list(Parser.parse(`
//  (import (scheme base))
//  ;(let ((a 1)) (+ a 1))
//  (define x 1)
//  (if x (begin 1 2) y)
//`));
//
//engine.expandToplevelProgram(forms)
//  .then(x => console.log(to_write(x)))
//  .catch(console.log)

// Example 2
//const engine = new Engine();
//console.log("=>", await engine.run(`
//(import (scheme base))
//(+ 1 2 3)
//`))

// Example 3
const engine = new Engine();
await engine.defineLibrary(List(Sym("counter")), `
(define-library (counter)
  (import (scheme base))
  (export get-count inc-count)
  (begin
    (define ct 0)
    (define (get-count) ct)
    (define (inc-count) (set! ct (+ ct 1)))))
  `);
console.log("")
console.log("=>", await engine.run(`
(import (scheme base) (counter))
(inc-count)
(get-count)
`))
