var fs = require('fs'),
    vm = require('vm');

var files = [
  "src/platforms/node/module_preamble.js",
  "src/version.js",
  "src/header.js",
  "src/system/class.js",
  "src/system/_writer.js",
  "src/system/_types.js",
  "src/system/error.js",
  "src/system/set.js",
  "src/system/values.js",
  "src/system/pair.js",
  "src/system/symbol.js",
  "src/system/char.js",
  "src/system/number.js",
  "src/system/port.js",
  "src/system/record.js",
  "src/system/enumeration.js",
  "src/system/hashtable.js",
  "src/system/syntax.js",
  "src/system/parser.js",
  "src/system/compiler.js",
  "src/system/pause.js",
  "src/system/call.js",
  "src/system/interpreter.js",
  "src/system/promise.js",
  "src/library/infra.js",
  "src/library/r6rs_lib.js",
  "src/library/js_interface.js",
  "src/library/extra_lib.js",
  "src/library/node_functions.js",
  "src/library/srfi.js",
  "src/platforms/node/module_postamble.js",
]

var sandbox = {
  require: require,
  process: process,
  exports: exports,
  console: console
};
vm.createContext(sandbox);

files.forEach(function(path){
  var js = fs.readFileSync(__dirname + "/../../../" + path);
  vm.runInContext(js, sandbox, {
    filename: "(biwa)"+path,
    displayErrors: true
  });
});
