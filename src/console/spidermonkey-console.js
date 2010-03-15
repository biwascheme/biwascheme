window = {
};

navigator = {
    userAgent: "console-spidermonkey",
};

document = {
    createElement: function(a) { 
	return {
	    appendChild: function() {
		return "dummy_document_createElement_appendChild";
	    },
	} ;
    },
    createTextNode: function(a) {
	return "dummy_document_createTextNode";
    },
    createEvent: function(a) {
	return "dummy_document_createEvent";
    },
    write: function(a) {
	return "dummy_document_wirte";
    },
    getElementById: function(a) {
	return "dummy_document_getElementById";
    },
};

Element = {
};

HTMLElement = {
    prototype: {},
};


Console = {}
Console.puts = function(str, no_newline) {
    print(str);
    if (!no_newline) 
	print("\n");
}

Console.p = function() {
    print.apply(this, arguments)
}

if(typeof(ev) != 'function')
    eval("function ev(str){ puts(str); return (new BiwaScheme.Interpreter()).evaluate(str); }");
