BiwaScheme.run = function(code, opts) {
  opts = opts || {};
  var intp = new BiwaScheme.Interpreter(function(e){
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

BiwaScheme.run_file = function(filename, encoding/*optional*/) {
  var enc = encoding || 'utf8';
  var src = require('fs').readFileSync(filename, enc);
  return BiwaScheme.run(src);
};

BiwaScheme.define_libfunc("load", 1, 1, function(ar) {
  var relpath = ar[0];
  BiwaScheme.assert_string(relpath);
  // assume path is relative to node_modules directory
  var filename = __dirname + "/../../../" + relpath;
  var code = require("fs").readFileSync(filename, "utf8");
  return BiwaScheme.run(code);
});

BiwaScheme.define_libfunc("js-load", 1, 1, function(ar) {
  var relpath = ar[0];
  BiwaScheme.assert_string(relpath);
  // assume path is relative to node_modules directory
  return require(__dirname + "/../../../" + relpath);
});

BiwaScheme.Port.current_error =
BiwaScheme.Port.current_output = new BiwaScheme.Port.CustomOutput(
  function (str) {
    require('util').print(str);
  }
);

var readline = require('readline');
BiwaScheme.Port.current_input = new BiwaScheme.Port.CustomInput(
  function (callback) {
    var rl = readline.createInterface(
      process.stdin,
      process.stdout
    );
    rl.on('line', function (line) {
      rl.close();
      callback(line);
    });
    rl.setPrompt('', 0);
    rl.prompt()
  }
);

for(x in BiwaScheme){
  exports[x] = BiwaScheme[x];
}
