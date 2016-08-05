var _ = require('underscore');
_.str = require('underscore.string');

var Console = {};
Console.puts = function(str, no_newline) {
  BiwaScheme.Port.current_output.put_string(str + (no_newline ? "" : "\n"))
};

Console.p = function() {
  [].slice.call(arguments).forEach(BiwaScheme.Port.current_output.put_string);
};

if(typeof(BiwaScheme) == "undefined") BiwaScheme = {};
BiwaScheme.on_node = true;
BiwaScheme._ = _;
BiwaScheme.Console = Console;
