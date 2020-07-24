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
  // (load scm-path)
  BiwaScheme.define_libfunc("load", 1, 1, function(ar) {
    var path = ar[0];
    BiwaScheme.assert_string(path);

    var fullpath;
    if (path[0] == "/" || /^\w:/.test(path))
      fullpath = path;
    else
      fullpath = process.cwd() + "/" + path;

    var code = require("fs").readFileSync(fullpath, "utf8");
    return BiwaScheme.run(code);
  });

  // (js-load js-path)
  BiwaScheme.define_libfunc("js-load", 1, 1, function(ar) {
    var path = ar[0];
    BiwaScheme.assert_string(path);

    var fullpath;
    if (path[0] == "/" || /^\w:/.test(path))
      fullpath = path;
    else
      fullpath = process.cwd() + "/" + path;

    return require(fullpath);
  });
};

export {run, run_file, node_setup};
