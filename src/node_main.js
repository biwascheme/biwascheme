var sys = require('sys'),
    fs = require('fs'),
    path = require('path'),
    
    optparse = require('optparse');

function Options(argv){
  var switches = [
    [      '--encoding', 'Specify encoding (default: utf8)'],
    ['-h', '--help', 'Shows help sections']
  ];

  var parser = new optparse.OptionParser(switches);

  parser.on('help', function() {
    sys.puts('Help');
  });

  this.args = parser.parse(argv);
}
var opts = new Options(process.argv.slice(2));

var intp = new BiwaScheme.Interpreter(function(e){
  sys.puts(Object.inspect(e));
  process.exit(1);
});

if(opts.args.size() >= 1){
  var src = fs.readFileSync(opts.args[0], 'utf8');
  intp.evaluate(src);
}
else{
  // TODO repl
  //sys.print("biwas > ");
}

