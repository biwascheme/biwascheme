//
// Library functions only work on Node.js
// see also: test/node_functions.js
//

(function(){
  if(BiwaScheme.on_node){
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

    if(BiwaScheme.on_node){
      BiwaScheme.define_libfunc.apply(null, args);
    }
    else{
      var func_name = args[0];
      var func = function(ar){
        throw new BiwaScheme.Error("the function '"+func_name+"' "+
          "is not supported in the browser "+
          "(works only on Node.js).");
      };
      args.pop();
      args.push(func);
      BiwaScheme.define_libfunc.apply(null, args);
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
    return BiwaScheme.undef;
  });

  //
  // Chapter 10 Command-line access and exit values
  //
  
  //(command-line)    procedure
  define_node_libfunc("command-line", 0, 0, function(ar){
    return BiwaScheme.List.apply(null, node.process.argv);
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
    return BiwaScheme.js_obj_to_alist(node.process.env);
  });

})();
