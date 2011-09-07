// loads server/node_preaamble.js first

BiwaScheme.run = function(filename) {
  var src = require('fs').readFileSync(filename, 'utf8');
  var intp = new BiwaScheme.Interpreter(function(e){
    if(e.stack){
      require('sys').puts(e.stack);
    }
    else{
      require('sys').puts(e);
    }

    process.exit(1);
  });
  return intp.evaluate(src);
};
BiwaScheme.define_libfunc("load", 1, 1, function(ar) {
  var relpath = ar[0];
  assert_string(relpath);
  // assume path is relative to node_modules directory
  return BiwaScheme.run(__dirname + "/../../../" + relpath);
});

for(x in BiwaScheme){
  exports[x] = BiwaScheme[x];
}
