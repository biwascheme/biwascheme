# BiwaScheme 1.0

Ideas for BiwaScheme 1.0

## New features

- r7rs library system
- hygenic macro
  - er-macro-transformer at least
  - syntax-rules maybe

This does not mean BiwaScheme 1.0 will be 100% r7rs compatible. Aside library system and syntax-rules, largest missing feature will be exception system. 

Nevertheless I'd like to give it 1.0 rather than 0.x because the above two are largest jump from 0.x and there will be some backward incompatibilities.

## JS Promise

BiwaScheme 1.0 will return the result by JS Promise. For example:

```js
const biwa = new BiwaScheme.Engine();
biwa.run("(+ 1 2)")
  .then(function(result) {
    console.log(result)
  })
  .catch(function(error) {
    console.error(error)
  })
```

## BiwaScheme.Engine

In BiwaScheme 0.x, scheme values are stored in `BiwaScheme.TopEnv`. It means all values are shared among instances of `BiwaScheme.Interpreter`.

For example, suppose embedding scheme code in a blog post. In BiwaScheme 0.x you need to be careful one program does not affect others. In BiwaScheme 1.0, toplevel values are stored in `BiwaScheme.Engine` and not shared with other engines.

## R7RS library system

BiwaScheme 0.x loads all the libraries in src/library/ by default. You will need explicit `(import ...)` in 1.0.

For convenience, there should be a way to import all the libraries available in 0.x (like `(import (biwa compat))` or something.)

## Development details

### async/await based expander

The new macro expander uses JS async/await. It is possible to write it
without async/await, but in that case all functions must convey continuation
(Continuation Passing Style.)

### Define library with JavaScript (tentative)

```js
const _BiwaExtra = List(Sym('biwa'), Sym('extra'));
const libBiwaExtra = Library.create(_BiwaExtra);
// case 1: normal function which returns normal value (= Basic API)
libBiwaExtra.environment.set(Sym('func1'), ([proc, engine]) => {
  return 123;
});
// case 2: async function which returns normal value
// (= Synchronous but non-blocking API. In BiwaScheme 0.x these are
// implemented with BiwaScheme.Pause)
libBiwaExtra.environment.set(Sym('func2'), async ([proc, engine]) => {
  await new Promise(resolve => setTimeout(resolve, 3000))
  return 123;
});
// case 3: normal function which returns js Promise (= Asynchronous API)
libBiwaExtra.environment.set(Sym('func3'), ([proc, engine]) => {
  return fetch(url);
});
// case 4: async function which returns js Promise
// (This is not recommended; Just `return someAsyncFunc()` is fine)
libBiwaExtra.environment.set(Sym('func4'), async ([proc, engine]) => {
  return await someAsyncFunc();
});
```

The difference between case 2 and 3 are important. In Scheme level,
`func2` returns a Scheme value while `func3` returns a JS Object (Promise).

### Key files

- src/
  - new_expander_dev.js : Temporary entry point
  - r7rs/ : Will contain R7RS small functions
  - system/
    - engine.js
    - expander/
      - core.js : Expanders for core syntaxes

### TODOs

- Implement basic constructs 
  - high priority
    + if
    - lambda
    - define
    - begin
    + quote
    + set!
    - call/cc
  - lower priority
    - define-record-type
    - parameterize
    - define-syntax, let-syntax, letrec-syntax, syntax-error
    - include, include-ci, if-expand, case-lambda
- Integrate `Compiler` and `Interpreter` with `Engine`
- Design how `import` and `include` works in browser and on Node.js
- Provide existing libraries as r7rs lib

### How to try the new expander

Edit those two files and `node src/new_expander_dev.js`

```diff
--- a/package.json
+++ b/package.json
@@ -1,4 +1,5 @@
 {
+    "type": "module",
     "name": "biwascheme",
     "description": "A practical Scheme interpreter written in JavaScript",
--- a/src/platforms/node/node_functions.js
+++ b/src/platforms/node/node_functions.js
@@ -11,8 +11,8 @@ import { run } from "./run.js"
 //

 const node = {
-  fs: require('fs'),
-  path: require('path'),
+//  fs: require('fs'),
+//  path: require('path'),
   process: process
 };
```
