var _ = require('underscore');
_.mixin(require('underscore.string'));

var Console = {};
Console.puts = function(str, no_newline) {
  require('sys').print(str);
  if (!no_newline) {
    require('sys').print("\n");
  }
};

Console.p = function() {
  require('sys').print.apply(this, arguments);
};
