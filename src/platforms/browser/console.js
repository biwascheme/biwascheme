Console = {}

Console.puts = function(str, no_newline) {
  var console;
  var text;
  console = $("#bs-console");
  if (console[0]) {
	text = _.escapeHTML(str + (no_newline ? "" : "\n"));
	var span = $("<span>");
	span.html(text.replace(/\n/g,"<br>").replace(/ /g,"&nbsp;"));
	console.append(span);
  }
};

Console.p = function (/*ARGS*/){
  Console.puts("p> "+_.map(_.toArray(arguments), BiwaScheme.inspect).join(" "));
};
