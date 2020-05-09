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
  Console.puts = function(str, no_newline) {
    BiwaScheme.Port.current_output.put_string(str + (no_newline ? "" : "\n"))
  };

  Console.p = function() {
    [].slice.call(arguments).forEach(BiwaScheme.Port.current_output.put_string);
  };

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

  BiwaScheme.Port.current_error =
  BiwaScheme.Port.current_output = new BiwaScheme.Port.CustomOutput(
    function (str) {
      process.stdout.write(str)
    }
  );

  var readline = require('readline');
  BiwaScheme.Port.current_input = new BiwaScheme.Port.CustomInput(
    function (callback) {
      var rl = readline.createInterface({
        input: process.stdin
      });
      rl.on('line', function (line) {
        rl.close();
        callback(line);
      });
      rl.setPrompt('', 0);
      rl.prompt()
    }
  );
};

export {run, run_file, node_setup};
