// loads server/node_preaamble.js first

var fs = require('fs');

BiwaScheme.run = function(filename) {
  var src = fs.readFileSync(filename, 'utf8');
  var intp = new BiwaScheme.Interpreter(function(e){
    sys.puts(e.stack);
    process.exit(1);
  });
  return intp.evaluate(src);
};

exports.BiwaScheme = BiwaScheme;
