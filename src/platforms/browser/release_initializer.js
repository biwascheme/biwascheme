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
    BiwaScheme.Port.current_error.put_string(e.message + "\n");
    if (dumper) {
      dumper.dump(state);
      dumper.dump_move(1);
    } else if (typeof(console) !== "undefined" && console.error) {
      console.error(e.message);
    } else {
      throw(e);
    }
  };

  var run = function(script) {
    var intp = new BiwaScheme.Interpreter(onError);
    try{
      intp.evaluate(script, function(){});
    }
    catch(e){
      onError(e);
    }
  };

  // Start user's program (old style)
  var script = $("script[src$='biwascheme.js']").html() ||
               $("script[src$='biwascheme-min.js']").html();
  if (script) run(script);

  // Start user's program (new style)
  $(function(){
    $("script[type='text/biwascheme']").each(function(){
      run($(this).html());
    });
  });
})();
