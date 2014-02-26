// release_initializer.js - read user's program and eval it (if it exists)
// This file is put on the end the lib/biwascheme.js.
$(document).ready(function () {
  "use strict";

  var dumper = (function () {
    if ($("#biwascheme-debugger")[0]) {
      return new BiwaScheme.Dumper($("#biwascheme-debugger")[0]);
    }
    else {
      return null;
    }
  })();

  // Error handler (show message to console div)
  var onError = function (e, state) {
    puts(e.message);

    if (dumper) {
      dumper.dump(state);
      dumper.dump_move(1);
    }
    else if (typeof console !== "undefined" && console.error) {
      console.error(e.message);
    }
    else {
      throw e;
    }
  };

  var interpret = function (script) {
    var interp = new BiwaScheme.Interpreter(onError);

    try {
      interp.evaluate(script, function () {});
    }
    catch (e) {
      onError(e);
    }
  };

  // Start user's program
  $("script[src*='biwascheme']").each(function (ix) {
    interpret($(this).html());
  });

  $("script.biwascheme").each(function (ix) {
    var elem = $(this);
    var source = elem.attr("src");

    if (source) {
      $.get(source, interpret, "text");
    }
    else {
      interpret(elem.html());
    }
  });
});
