Console = {}

Console.puts = function(str, no_newline) {
  var console;
  var text;
  console = $("#bs-console");
  if (console) {
	text = underscore.escapeHTML(str + (no_newline ? "" : "\n"));
	var span = $("<span>");
	span.html(text.replace(/\n/g,"<br>").replace(/ /g,"&nbsp;"));
	console.append(span);
  }
}

Console.p = function (/*ARGS*/){
  Console.puts("p> "+underscore.map(underscore.toArray(arguments), BiwaScheme.inspect).join(" "));
}
