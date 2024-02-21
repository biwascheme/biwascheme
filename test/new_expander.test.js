//
// Tests for the new expander
// Eventually should be merged into the main tests
//
import { Engine } from "../src/system/engine.js";
import { List } from "../src/system/pair.js";
import { Sym } from "../src/system/symbol.js";
import { to_write } from "../src/system/_writer.js";
import { Port } from "../src/system/port.js";
import "../src/library/r6rs_lib.js"

let written = [];
Port.current_output = new Port.CustomOutput(
  function (str) {
    written.push(str);
  }
);

test('Evaluating scheme program', async () => {
  const engine = new Engine();
  const result = await engine.run(`
    (import (scheme base))
    (+ 1 2 3)
  `);
  expect(result).toEqual(6);
});

test('Defining a library', async () => {
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
  let result = await engine.run(`
    (import (scheme base) (counter))
    (inc-count)
    (get-count)
  `);
  expect(result).toEqual(1);
});

test('Library local name', async () => {
  const engine = new Engine();
  await engine.defineLibrary(List(Sym("utils")), `
    (define-library (utils)
      (import (scheme base))
      (export log)
      (begin
        (define msgs '())
        (define (log msg) (set! msgs (cons msg msgs)))))
  `);
  let result = await engine.run(`
    (import (utils) (scheme base))
    (define msgs "ok")
    (log "hello")  ; Does not overwrite the our msgs
    msgs
  `);
  expect(result).toEqual("ok");
});

test('ER macro', async () => {
  written = [];
  const engine = new Engine();
  let result = await engine.run(`
    (import (scheme base) (scheme cxr) (scheme write) (biwascheme er-macro))
    (define-syntax assert-equal
      (er-macro-transformer
        (lambda (form rename compare)
          \`(unless (equal? ,(cadr form) ,(caddr form))
            (display "failed: ")
            (write ',(cdr form))))))
    (assert-equal (+ 1 2) 4)
  `);
  expect(written).toEqual(["failed: ", "((+ 1 2) 4)"]);
});

test('Exporting a macro', async () => {
  written = [];
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
  let result = await engine.run(`
    (import (scheme base) (assert)) ; You don't need (scheme write) to use the macro.
    (assert-equal (+ 1 2) 4)
  `);
  expect(written).toEqual(["failed: ", "((+ 1 2) 4)"]);
});

test('Avoid name collision', async () => {
  written = [];
  const engine = new Engine();
  let result = await engine.run(`
    (import (scheme base) (scheme cxr) (scheme write) (biwascheme er-macro))
    (define-syntax days
      (er-macro-transformer
        (lambda (form rename compare)
          \`(,(rename list) 1 5 25))))
    ; It should work properly even if the name 'list' is overridden
    (let ((list '()))
      (days))
  `);
  expect(to_write(result)).toEqual("(1 5 25)");
});
