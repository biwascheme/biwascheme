var fs = require('fs'),
    vm = require('vm');

var node_files = JSON.parse(fs.readFileSync(__dirname + '/../../../FILES.json'))["node"];

var sandbox = {
  require: require,
  process: process,
  exports: exports,
  console: console
};
vm.createContext(sandbox);

node_files.forEach(function(path){
  var js = fs.readFileSync(__dirname + "/../../../" + path);
  vm.runInContext(js, sandbox, {
    filename: "(biwa)"+path,
    displayErrors: true
  });
});
