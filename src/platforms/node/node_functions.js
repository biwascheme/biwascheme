import * as _ from "../../deps/underscore-1.10.2-esm.js"
import { undef } from "../../header.js";
import { define_libfunc, assert_string } from "../../library/infra.js"; 
import { BiwaError } from "../../system/error.js"
import { List, js_obj_to_alist } from "../../system/pair.js"
import { run } from "./run.js"
//
// Library functions only work on Node.js
// see also: test/node_functions.js
//

const node = {
  fs: require('fs'),
  path: require('path'),
  process: process
};

//
// Chapter 9 File System
//

//(file-exists? filename)    procedure 
define_libfunc("file-exists?", 1, 1, function(ar){
  assert_string(ar[0]);
  return node.fs.existsSync(ar[0]);
});

//(delete-file filename)    procedure 
define_libfunc("delete-file", 1, 1, function(ar){
  assert_string(ar[0]);
  node.fs.unlinkSync(ar[0]);
  return undef;
});

//
// Chapter 10 Command-line access and exit values
//

//(command-line)    procedure
define_libfunc("command-line", 0, 0, function(ar){
  return List.apply(null, node.process.argv);
});

//(exit)    procedure 
//(exit obj)    procedure
define_libfunc("exit", 0, 1, function(ar){
  var obj = ar[0];
  var code = _.isUndefined(obj) ? 0 :
             (obj === false)    ? 1 :
             Number(obj);

  node.process.exit(code);
});

//
// srfi-98 (get-environment-variable)
//

// (get-environment-variable name) -> string or #f
define_libfunc("get-environment-variable", 1, 1, function(ar){
  assert_string(ar[0]);
  var val = node.process.env[ar[0]];
  return _.isUndefined(val) ? false : val;
});

// (get-environment-variables) -> alist of string (("key" . "value"))
define_libfunc("get-environment-variables", 0, 0, function(ar){
  return js_obj_to_alist(node.process.env);
});

//
// Extras
//

// (load scm-path)
define_libfunc("load", 1, 1, function(ar) {
  var path = ar[0];
  assert_string(path);

  var fullpath;
  if (path[0] == "/" || /^\w:/.test(path))
    fullpath = path;
  else
    fullpath = process.cwd() + "/" + path;

  var code = require("fs").readFileSync(fullpath, "utf8");
  return run(code);
});

// (js-load js-path)
define_libfunc("js-load", 1, 1, function(ar) {
  var path = ar[0];
  assert_string(path);

  var fullpath;
  if (path[0] == "/" || /^\w:/.test(path))
    fullpath = path;
  else
    fullpath = process.cwd() + "/" + path;

  return require(fullpath);
});

// (node-require "fs")
define_libfunc("node-require", 1, 1, function(ar) {
  var name = ar[0];
  assert_string(name);

  return require(name);
});
