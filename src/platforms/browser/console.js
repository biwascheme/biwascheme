Console = {}

BiwaScheme.Port.current_error =
BiwaScheme.Port.current_output = new BiwaScheme.Port.CustomOutput(
  function (str) {
    var console;
    var text;
    console = $("#bs-console");
    if (console[0]) {
    	text = _.escape(str);
    	var span = $("<span>");
    	span.html(text.replace(/\n/g,"<br>").replace(/ /g,"&nbsp;"));
    	console.append(span);
    }
  }
);

BiwaScheme.Port.current_input = new BiwaScheme.Port.CustomInput(
  function (callback) {
    var form = $("<form/>");
    form.html("<input id='webscheme-read-line' type='text'><input type='submit' value='ok'>");
    $("#bs-console").append(form);
    form.submit(function(){
      var input = $("#webscheme-read-line").val();
      form.remove();
      callback(input);
      return false;
    });
  }
);


Console.puts = function(str, no_newline) {
  BiwaScheme.Port.current_output.put_string(str + (no_newline ? "" : "\n"))
};

Console.p = function (/*ARGS*/){
  BiwaScheme.Port.current_output.put_string(
    "p> "+_.map(_.toArray(arguments), BiwaScheme.inspect).join(" ")
  );
};
