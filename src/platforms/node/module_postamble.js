BiwaScheme.run = function(code) {
  var intp = new BiwaScheme.Interpreter(function(e){
    if(e.stack){
      require('util').puts(e.stack);
    }
    else{
      require('util').puts(e);
    }

    process.exit(1);
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
  assert_string(relpath);
  // assume path is relative to node_modules directory
  var filename = __dirname + "/../../../" + relpath;
  var code = require("fs").readFileSync(filename, "utf8");
  return BiwaScheme.run(code);
});

BiwaScheme.define_libfunc("js-load", 1, 1, function(ar) {
  var relpath = ar[0];
  assert_string(relpath);
  // assume path is relative to node_modules directory
  return require(__dirname + "/../../../" + relpath);
});

for(x in BiwaScheme){
  exports[x] = BiwaScheme[x];
}
