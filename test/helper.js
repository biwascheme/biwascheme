import BiwaScheme from "../src/main.js"

// Default error handler
var on_error = function(e){
  console.warn(e);
  throw e;
}

// Evaluates `str` and returns the resulting Scheme value
function evaluate(str, func) {
  return (new BiwaScheme.Interpreter(on_error)).evaluate(str, func||new Function());
}

// Evaluates `str` and returns expectation
function ev(str, func) {
  const v = evaluate(str, func);
  return expect(v);
}

// Same as ev but makes the result into a string by to_write
function ew(str, func) {
  const v = evaluate(str, func);
  return expect(BiwaScheme.to_write(v));
}

export { ev, ew }
