Console = {}

Console.puts = function(str, no_newline) {
    var text = (str + (no_newline ? "" : "\n")).escapeHTML();
    span = document.createElement("span");
    span.innerHTML = text.replace(/\n/g,"<br>").replace(/ /g,"&nbsp;");
    $('bs-console').insert(span);
}

Console.p = function (/*ARGS*/){
    Console.puts("p> "+$A(arguments).map(Object.inspect).join(" "));
}
