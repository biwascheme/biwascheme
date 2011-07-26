var underscore = require('underscore');
underscore.mixin(require('underscore.string'));

var window = {
};

var navigator = {
  userAgent: "console-nodejs"
};

var document = {
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

var Element = {
};

var HTMLElement = {
  prototype: {}
};


var Console = {};
Console.puts = function(str, no_newline) {
  require('sys').print(str);
  if (!no_newline) {
    require('sys').print("\n");
  }
};

Console.p = function() {
  require('sys').print.apply(this, arguments);
};
