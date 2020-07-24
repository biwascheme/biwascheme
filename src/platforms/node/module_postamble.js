import Interpreter from "../../system/interpreter.js"

const run = function(code, opts) {
  opts = opts || {};
  var intp = new Interpreter(function(e){
    if(!opts["no_print"]) {
      if(e.stack){
        console.error(e.stack);
      }
      else{
        console.error(e.toString ? e.toString() : e);
      }
    }

    throw e;
  });
  return intp.evaluate(code);
};

const run_file = function(filename, encoding/*optional*/) {
  var enc = encoding || 'utf8';
  var src = require('fs').readFileSync(filename, enc);
  return run(src);
};

const node_setup = (BiwaScheme) => {
};

export {run, run_file, node_setup};
