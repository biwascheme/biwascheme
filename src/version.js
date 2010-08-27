var BiwaScheme = BiwaScheme || {};

BiwaScheme.Version = "0.5.4.2";
BiwaScheme.GitCommit = "127df5097357cb8aa3414039ca4b30233434e27d";

  // FIXME: used by js-load. should be moved
BiwaScheme.require = function(src, check, proc){
  var script = document.createElement('script')
  script.src = src;
  document.body.appendChild(script);

  var checker = new Function("return !!(" + check + ")");

  if(checker()) proc();
  else          setTimeout(function(){ checker() ? proc() : setTimeout(arguments.callee, 10); }, 10);
};
