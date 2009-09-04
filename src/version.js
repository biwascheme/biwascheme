var BiwaScheme = {
  Version: 0.5,

  // FIXME: used by js-load. should be moved
  require: function(src, check, proc){
    var script = document.createElement('script')
    script.src = src;
    document.body.appendChild(script);

    var checker = new Function("return !!(" + check + ")");

    if(checker()) proc();
    else          setTimeout(function(){ checker() ? proc() : setTimeout(arguments.callee, 10); }, 10);
  }
};


