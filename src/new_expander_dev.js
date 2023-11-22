// Acknowledgement: https://github.com/nyuichi/r7expander
//
// How to run
//   $ node ./src/new_expander_dev.js
// See https://github.com/biwascheme/biwascheme/pull/192#issuecomment-673534970
// if it doesn't work
import { inspect } from "./system/_writer.js";
import Call from "./system/call.js";
import Parser from "./system/parser.js";
import { List, array_to_list } from "./system/pair.js";
import { Sym } from "./system/symbol.js";
import { Library } from "./system/expander/library.js"
import { Engine } from "./system/engine.js";
import { Port } from "./system/port.js";

import "./library/r6rs_lib.js"

// copied from main-node.js
import { current_input, current_output, current_error } from "./platforms/node/default_ports.js"
Port.current_input = current_input;
Port.current_output = current_output;
Port.current_error = current_error;

// Example: Exporting a macro
// (see test/new_expander_test.js for more examples)
const engine = new Engine();
await engine.defineLibrary(List(Sym("assert")), `
 (define-library (assert)
   (import (scheme base) (biwascheme er-macro)  (scheme cxr) (scheme write))
   (export assert-equal)
   (begin
     (define-syntax assert-equal
       (er-macro-transformer
         (lambda (form rename compare)
           \`(unless (equal? ,(cadr form) ,(caddr form))
             (,(rename display) "failed: ")
             (,(rename write) ',(cdr form))))))))
   `);
console.log("")
console.log("=>", await engine.run(`
(import (scheme base)(assert))
(assert-equal (+ 1 2) 4)
`))