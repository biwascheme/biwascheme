// TODO load underscore and underscore.js

var Console = {};
Console.puts = function(str, no_newline) {
  print(str);
  if (!no_newline) {
    print("\n");
  }
};

Console.p = function() {
  print.apply(this, arguments);
};

if(typeof(ev) != 'function')
    eval("function ev(str){ puts(str); return (new BiwaScheme.Interpreter()).evaluate(str); }");
