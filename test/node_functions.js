//
// test/node_functions.js
//
// $ npm run node-test
var BiwaScheme = require('..'),
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
var puts = console.log;

puts("Running tests...");

var tests = {
  // R6RS stdlib 9
  "file-exists? (#t)": function(){
    assert.ok(ev('(file-exists? "Makefile")'));
  },

  "file-exists? (#f)": function(){
    assert.ok(!ev('(file-exists? "a file do not exist")'));
  },

  "delete-file": function(){
    var test_file_path = "./test/test_file_for_delete_file.txt";

    fs.writeFileSync(test_file_path, "hi");

    ev('(delete-file "'+test_file_path+'")');

    assert.ok(!fs.existsSync(test_file_path));
  },

  // R6RS stdlib 10
  "command-line": function(){
    var list = ev('(command-line)');
    assert.ok(BiwaScheme.isList(list));
    list.foreach(function(item){
      assert.ok(_.isString(item));
    });
  },

  "exit": function(){
    var orig_exit = process.exit;
    var expected_code;
    process.exit = function(given_code){
      assert.strictEqual(given_code, expected_code);
    };

    expected_code = 0;   ev("(exit)");
    expected_code = 0;   ev("(exit 0)");
    expected_code = 123; ev("(exit 123)");
    expected_code = 1;   ev("(exit #f)");

    process.exit = orig_exit;
  },

  // Load
  "load": function(){
    var result = ev('(load "test/node_functions/foo.scm") FOO');
    assert.equal(result, 1);
  },

  "js-load": function(){
    var result = ev('(js-load "test/node_functions/foo.js")');
    assert.equal(result.FOO, 2);
  },

  // SRFI 98
  "get-environment-variable": function(){
    var path = ev('(get-environment-variable "PATH")');
    assert.ok(_.isString(path));
    assert.equal(ew('(get-environment-variable "NON EXIST")'),
                 "#f");
  },

  "get-environment-variables": function(){
    var first_env = ev('(car (get-environment-variables))');
    assert.ok(BiwaScheme.isPair(first_env));
    assert.ok(_.isString(first_env.car));
    assert.ok(_.isString(first_env.cdr));
  }
};

tests.forEach(function(func, name){
  puts("- "+name);
  func();
});

puts("test ok");
