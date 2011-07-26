if(typeof(ev) != 'function') {
  eval("function ev(str){ puts(str); return (new BiwaScheme.Interpreter()).evaluate(str); }");
}
