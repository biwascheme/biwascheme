import BiwaScheme from "../src/main.js"

var on_error = function(e){
  console.warn(e);
  throw e;
}
function ev(str, func) {
  return expect((new BiwaScheme.Interpreter(on_error)).evaluate(str, func||new Function()));
}

export { ev }
