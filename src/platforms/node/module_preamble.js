var _ = require('underscore');
_.str = require('underscore.string');

var Console = {};
Console.puts = function(str, no_newline) {
  require('util').print(str);
  if (!no_newline) {
    require('util').print("\n");
  }
};

Console.p = function() {
  require('util').print.apply(this, arguments);
};

if(typeof(BiwaScheme) == "undefined") BiwaScheme = {};
BiwaScheme.on_node = true;
BiwaScheme._ = _;
BiwaScheme.Console = Console;
