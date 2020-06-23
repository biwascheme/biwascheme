import * as _ from "../deps/underscore-1.10.2-esm.js"
import { undef } from "../header.js";
import Platform from "../platforms/platform.js"
import { define_libfunc, assert_string } from "./infra.js"; 
import { BiwaError } from "../system/error.js"
import { List, js_obj_to_alist } from "../system/pair.js"
//
// Library functions only work on Node.js
// see also: test/node_functions.js
//

  if(Platform.isNode){
    var node = {
      fs: require('fs'),
      path: require('path'),
      process: process
    };
  }

  // Defines library functions which only works on Node.
  // - On Node: same as define_libfunc
  // - On Browser: defines a stub libfunc which just raises Error
  var define_node_libfunc = function(/*arguments*/){
    var args = _.toArray(arguments);

    if(Platform.isNode){
      define_libfunc.apply(null, args);
    }
    else{
      var func_name = args[0];
      var func = function(ar){
        throw new BiwaError("the function '"+func_name+"' "+
          "is not supported in the browser "+
          "(works only on Node.js).");
      };
      args.pop();
      args.push(func);
      define_libfunc.apply(null, args);
    }
  };

  //
  // Chapter 9 File System
  //

  //(file-exists? filename)    procedure 
  define_node_libfunc("file-exists?", 1, 1, function(ar){
    assert_string(ar[0]);
    return node.fs.existsSync(ar[0]);
  });

  //(delete-file filename)    procedure 
  define_node_libfunc("delete-file", 1, 1, function(ar){
    assert_string(ar[0]);
    node.fs.unlinkSync(ar[0]);
    return undef;
  });

  //
  // Chapter 10 Command-line access and exit values
  //
  
  //(command-line)    procedure
  define_node_libfunc("command-line", 0, 0, function(ar){
    return List.apply(null, node.process.argv);
  });

  //(exit)    procedure 
  //(exit obj)    procedure
  define_node_libfunc("exit", 0, 1, function(ar){
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
  define_node_libfunc("get-environment-variable", 1, 1, function(ar){
    assert_string(ar[0]);
    var val = node.process.env[ar[0]];
    return _.isUndefined(val) ? false : val;
  });

  // (get-environment-variables) -> alist of string (("key" . "value"))
  define_node_libfunc("get-environment-variables", 0, 0, function(ar){
    return js_obj_to_alist(node.process.env);
  });
