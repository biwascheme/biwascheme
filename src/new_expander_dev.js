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


// Example 3: defining a library
// const engine = new Engine();
// await engine.defineLibrary(List(Sym("counter")), `
// (define-library (counter)
//   (import (scheme base))
//   (export get-count inc-count)
//   (begin
//     (define ct 0)
//     (define (get-count) ct)
//     (define (inc-count) (set! ct (+ ct 1)))))
//   `);
// console.log("")
// console.log("=>", await engine.run(`
// (import (scheme base) (counter))
// (inc-count)
// (get-count)
// `))

// Example 4: library-local name
//const engine = new Engine();
//await engine.defineLibrary(List(Sym("utils")), `
//(define-library (utils)
//  (import (scheme base))
//  (export log)
//  (begin
//    (define msgs '())
//    (define (log msg) (set! msgs (cons msg msgs)))))
//  `);
//console.log("")
//console.log("=>", await engine.run(`
//(import (utils) (scheme base))
//(define msgs "ok")
//(log "hello")  ; Does not overwrite the our msgs
//msgs
//`))

// Example 5: ER macro
//const engine = new Engine();
//console.log("=>", await engine.run(`
//(import (scheme base) (scheme cxr) (scheme write) (biwascheme er-macro))
//(define-syntax assert-equal
//  (er-macro-transformer
//    (lambda (form rename compare)
//      \`(unless (equal? ,(cadr form) ,(caddr form))
//        (display "failed: ")
//        (write ',(cdr form))))))
//(assert-equal (+ 1 2) 4)
//`))

// Example 6: Exporting a macro
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