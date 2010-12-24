var BiwaScheme = BiwaScheme || {};

BiwaScheme.Version = "0.5.5.1";
BiwaScheme.GitCommit = "a7952b0d1296d1c906486e460b34b0df728b0229";

  // FIXME: used by js-load. should be moved
BiwaScheme.require = function(src, check, proc){
  var script = document.createElement('script')
  script.src = src;
  document.body.appendChild(script);

  var checker = new Function("return !!(" + check + ")");

  if(checker()) proc();
  else          setTimeout(function(){ checker() ? proc() : setTimeout(arguments.callee, 10); }, 10);
};
