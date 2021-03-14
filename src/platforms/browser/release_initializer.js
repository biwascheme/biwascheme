import Dumper from "./dumper.js"

//
// release_initializer.js - read user's program and eval it (if it exists)
//

const execute_user_program = function() {
  const dumper = null;
  const debug_area = document.querySelector("#biwascheme-debugger");
  if (debug_area) {
    dumper = new Dumper(debug_area);
  }

  // Error handler (show message to console div)
  const onError = function(e, state){
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

  const run = function(script) {
    const intp = new BiwaScheme.Interpreter(onError);
    try{
      intp.evaluate(script, function(){});
    }
    catch(e){
      onError(e);
    }
  };

  // Start user's program (old style)
  let script = "";
  for (const s of document.querySelectorAll("script[src$='biwascheme.js']")) {
    script += s.innerHTML;
  }
  for (const s of document.querySelectorAll("script[src$='biwascheme-min.js']")) {
    script += s.innerHTML;
  }

  if (script.length > 0) run(script);

  // Start user's program (new style)
  window.addEventListener('DOMContentLoaded', function(){
    for (const s of document.querySelectorAll("script[type='text/biwascheme']")) {
      run(s.innerHTML);
    }
  });
};

export { execute_user_program }
