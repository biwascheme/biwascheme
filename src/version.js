var BiwaScheme = BiwaScheme || {};

BiwaScheme.Version = "0.5.4.2";
BiwaScheme.GitCommit = "e73fa7c7f36a434a38875635e40e67cdad88bc85";

  // FIXME: used by js-load. should be moved
BiwaScheme.require = function(src, check, proc){
  var script = document.createElement('script')
  script.src = src;
  document.body.appendChild(script);

  var checker = new Function("return !!(" + check + ")");

  if(checker()) proc();
  else          setTimeout(function(){ checker() ? proc() : setTimeout(arguments.callee, 10); }, 10);
};
