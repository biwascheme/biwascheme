var _ = require('underscore');
_.str = require('underscore.string');

var Console = {};
Console.puts = function(str, no_newline) {
  process.stdout.write(str);
  if (!no_newline) {
    process.stdout.write("\n");
  }
};

Console.p = function() {
  process.stdout.write(this, arguments);
};

if(typeof(BiwaScheme) == "undefined") BiwaScheme = {};
BiwaScheme.on_node = true;
BiwaScheme._ = _;
BiwaScheme.Console = Console;
