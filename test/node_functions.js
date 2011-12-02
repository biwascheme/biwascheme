//
// test/node_functions.js
//
// $ npm install underscore .
var BiwaScheme = require('biwascheme'),
    _ = require('underscore'),
    assert = require('assert'),
    fs = require('fs'),
    path = require('path'),
    util = require('util');

var ev = function(src){
  return BiwaScheme.run(src);
};
var ew = function(src){
  return BiwaScheme.to_write(BiwaScheme.run(src));
};
var puts = util.puts;

puts("Running tests...");

// R6RS stdlib 9

puts("- file-exists? (#t)");
assert.ok(ev('(file-exists? "Makefile")'));

puts("- file-exists? (#f)");
assert.ok(!ev('(file-exists? "a file do not exist")'));

puts("- delete-file");
var test_file_path = "./test/test_file_for_delete_file.txt"
fs.writeFileSync(test_file_path, "hi");
ev('(delete-file "'+test_file_path+'")');
assert.ok(!path.existsSync(test_file_path));

// R6RS stdlib 10

puts("- command-line");
var list = ev('(command-line)');
assert.ok(BiwaScheme.isList(list));
list.foreach(function(item){
  assert.ok(_.isString(item));
});

puts("- exit");
var orig_exit = process.exit;
var expected_code;
process.exit = function(given_code){
  assert.strictEqual(given_code, expected_code);
};
expected_code = 0;   ev("(exit)");
expected_code = 0;   ev("(exit 0)");
expected_code = 123; ev("(exit 123)");
expected_code = 1;   ev("(exit #f)");

util.puts("test ok");
