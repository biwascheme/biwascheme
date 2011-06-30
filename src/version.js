var BiwaScheme = BiwaScheme || {};

BiwaScheme.Version = "0.5.5.2";
BiwaScheme.GitCommit = "7b9130dec4df7d110483f2525c52c7fd3fd16064";

  // FIXME: used by js-load. should be moved
BiwaScheme.require = function(src, check, proc){
  var script = document.createElement('script')
  script.src = src;
  document.body.appendChild(script);

  var checker = new Function("return !!(" + check + ")");

  if(checker()) proc();
  else          setTimeout(function(){ checker() ? proc() : setTimeout(arguments.callee, 10); }, 10);
};
