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
    } else if (typeof(console) !== "undefined" && console.error) {
      console.error(e.message);
    } else {
      throw(e);
    }
  };

  // Start user's program
  var intp = new BiwaScheme.Interpreter(onError);

    var scripts = $("script[type=\"text/scheme\"]");
    console.log(scripts);
    scripts.each(function(index, script) {
        var scheme = $.ajax({
            type: "GET",
            url: $(script).attr('src'),
            async: false
        }).responseText;
        try{
            intp.evaluate(scheme, function(){});
        }
        catch(e){
            onError(e);
        }
    });
})();
