//
// release_initializer.js - read user's program and eval it (if it exists)
//
// This file is put on the end the lib/biwascheme.js.
//
(function(){ //local namespace
  var dumper = null;
  if ($("#biwascheme-debugger")[0]) {
    dumper = new BiwaScheme.Dumper($("#biwascheme-debugger")[0]);
  }

  // Error handler (show message to console div)
  var onError = function(e, state){
    puts(e.message);
    if (dumper) {
      dumper.dump(state);
      dumper.dump_move(1);
    } else {
      throw(e);
    }
  };

  // Start user's program
  var script = $("script[src$='biwascheme.js']").html() ||
               $("script[src$='biwascheme-min.js']").html();
  if (script) {
    var intp = new BiwaScheme.Interpreter(onError);
    try{
      intp.evaluate(script, function(){});
    }
    catch(e){
      onError(e);
    }
  }
})();
