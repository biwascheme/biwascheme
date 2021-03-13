import * as _ from "../deps/underscore-1.10.2-esm.js"
import { nil, undef } from "../header.js";
import { define_libfunc, alias_libfunc, define_syntax, define_scmfunc,
         assert_number, assert_integer, assert_real, assert_between, assert_string,
         assert_char, assert_symbol, assert_port, assert_pair, assert_list,
         assert_function, assert_closure, assert_procedure, assert_date, assert, deprecate } from "./infra.js"; 
import { isSymbol, isClosure } from "../system/_types.js"
import { inspect } from "../system/_writer.js"
import { BiwaError } from "../system/error.js"
import Interpreter from "../system/interpreter.js"
import { Pair, isList, array_to_list, js_obj_to_alist, alist_to_js_obj } from "../system/pair.js"
import Pause from "../system/pause.js"
import { BiwaSymbol, Sym } from "../system/symbol.js"

//
// interface to javascript
//

// Rebind uses of eval to the global scope, to avoid polluting downstream code.
// See: https://github.com/rollup/rollup/wiki/Troubleshooting#avoiding-eval
let eval2 = eval;

define_libfunc("js-eval", 1, 1, function(ar){
  return eval2(ar[0]);
});
define_libfunc("js-ref", 2, 2, function(ar){
  if(_.isString(ar[1])){
    return ar[0][ar[1]];
  }
  else{
    assert_symbol(ar[1]);
    return ar[0][ar[1].name];
  }
});
define_libfunc("js-set!", 3, 3, function(ar){
  assert_string(ar[1]);
  ar[0][ar[1]] = ar[2];
  return undef;
});

// (js-call (js-eval "Math.pow") 2 4)
define_libfunc("js-call", 1, null, function(ar){
  var js_func = ar.shift();
  assert_function(js_func);

  var receiver = null;
  return js_func.apply(receiver, ar);
});
// (js-invoke (js-new "Date") "getTime")
define_libfunc("js-invoke", 2, null, function(ar){
  var js_obj = ar.shift();
  var func_name = ar.shift();
  if(!_.isString(func_name)){
    assert_symbol(func_name);
    func_name = func_name.name;
  }
  if(js_obj[func_name])
    return js_obj[func_name].apply(js_obj, ar);
  else
    throw new Error("js-invoke: function "+func_name+" is not defined");
});

// Short hand for JavaScript method call.
//
// (js-invocation obj '(foo 1 2 3))  ;=> obj.foo(1,2,3)
// (js-invocation obj '(foo 1 2 3)   ;=> obj.foo(1,2,3)
//                    'bar           ;      .bar
//                    '(baz 4 5))    ;      .baz(4,5)
// (js-invocation 'Math '(pow 2 3))  ;=> Math.pow(2,3)
//
// It also converts
//   (lambda (e) ...) to
//   (js-closure (lambda (e) ...))
//   and
//   '((a . b) (c . 4)) to
//   {a: "b", c: 4}
//
define_libfunc("js-invocation", 2, null, function(ar, intp){
  var receiver = ar.shift();
  // TODO: convert lambdas by js-closure 
  if(isSymbol(receiver)){
    receiver = eval2(receiver.name); //XXX: is this ok?
  }

  var v = receiver;

  // Process each method call
  _.each(ar, function(callspec){
      if(isSymbol(callspec)){
        // Property access
        v = v[callspec.name];
      }
      else if(isList(callspec)){
        // Method call
        var args = callspec.to_array();

        assert_symbol(args[0]);
        var method = args.shift().name;

        // Convert arguments
        args = _.map(args, function(arg){
            if(isClosure(arg)){
              // closure -> JavaScript funciton
              return js_closure(arg, intp);
            }
            else if(isList(arg)){
              // alist -> JavaScript Object
              var o = {};
              arg.foreach(function(pair){
                  assert_symbol(pair.car);
                  o[pair.car.name] = pair.cdr;
                });
              return o;
            }
            else
              return arg;
          });

        // Call the method
        if(!_.isFunction(v[method])){
          throw new BiwaError("js-invocation: the method `"+method+"' not found");
        }
        v = v[method].apply(v, args);
      }
      else{
        // (wrong argument)
        throw new BiwaError("js-invocation: expected list or symbol for callspec but got " + inspect(callspec));
      }
    });

  return v;
});

// TODO: provide corresponding macro ".." 
define_syntax("..", function(x){
  if (x.cdr == nil) {
    throw new Error("malformed ..");
  }
  return new Pair(Sym("js-invocation"), x.cdr);
});

// (js-new (js-eval "Date") 2005 1 1)
// (js-new (js-eval "Draggable") elem 'onEnd (lambda (drg) ...))
//   If symbol is given, following arguments are converted to 
//   an js object. If any of them is a scheme closure,
//   it is converted to js function which invokes that closure.
//
// (js-new "Date" 2005 1 1)
//   You can pass javascript program string for constructor.
define_libfunc("js-new", 1, null, function(ar, intp){
  // make js object from key-value pair
  var array_to_obj = function(ary){
    if((ary.length % 2) != 0)
      throw new Error("js-new: odd number of key-value pair");

    var obj = {};
    for(var i=0; i<ary.length; i+=2){
      var key = ary[i], value = ary[i+1];
      assert_symbol(key);
      if(isClosure(value))
        value = js_closure(value, intp);

      obj[key.name] = value;
    }
    return obj;
  };

  var ctor = ar.shift();
  if (_.isString(ctor)) ctor = eval2(ctor);

  if(ar.length == 0){
    return new ctor();
  }
  else{
    // pack args to js object, if symbol appears
    var args = [];
    for(var i=0; i<ar.length; i++){
      if(ar[i] instanceof BiwaSymbol){
        args.push(array_to_obj(ar.slice(i)));
        break;
      }
      else{
        args.push(ar[i]);
      }
    }
    // Run `new ctor(...args)`;
    return new (Function.prototype.bind.apply(ctor, [null].concat(args)))();
  }
});

// (js-obj "foo" 1 "bar" 2)
// -> {"foo": 1, "bar": 2}
define_libfunc("js-obj", 0, null, function(ar){
  if(ar.length % 2 != 0){
    throw new Error("js-obj: number of arguments must be even");
  }

  var obj = {};
  for(i=0; i<ar.length/2; i++){
    assert_string(ar[i*2]);
    obj[ar[i*2]] = ar[i*2+1];
  }
  return obj;
});

const js_closure = function(proc, intp){
  var intp2 = new Interpreter(intp);
  return function(/*args*/){
    return intp2.invoke_closure(proc, _.toArray(arguments));
  };
};
// (js-closure (lambda (event) ..))
// Returns a js function which executes the given procedure.
//
// Example
//   (add-handler! ($ "#btn") "click" (js-closure on-click))
define_libfunc("js-closure", 1, 1, function(ar, intp){
  assert_closure(ar[0]);
  return js_closure(ar[0], intp);
});

define_libfunc("js-null?", 1, 1, function(ar){
  return ar[0] === null;
});

define_libfunc("js-undefined?", 1, 1, function(ar){
  return ar[0] === undefined;
});

define_libfunc("js-function?", 1, 1, function(ar){
  return _.isFunction(ar[0]);
});

define_libfunc("js-array-to-list", 1, 1, function(ar){
  deprecate("js-array-to-list", "1.0", "js-array->list");
  return array_to_list(ar[0]);
});

define_libfunc("js-array->list", 1, 1, function(ar){
  return array_to_list(ar[0]);
});

define_libfunc("list-to-js-array", 1, 1, function(ar){
  deprecate("list-to-js-array", "1.0", "list->js-array");
  return ar[0].to_array();
});

define_libfunc("list->js-array", 1, 1, function(ar){
  return ar[0].to_array();
});

define_libfunc("alist-to-js-obj", 1, 1, function(ar) {
  deprecate("alist-to-js-obj", "1.0", "alist->js-obj");
  return alist_to_js_obj(ar[0]);
});

define_libfunc("alist->js-obj", 1, 1, function(ar) {
  assert_list(ar[0]);
  return alist_to_js_obj(ar[0]);
});

define_libfunc("js-obj-to-alist", 1, 1, function(ar) {
  deprecate("js-obj-to-alist", "1.0", "js-obj->alist");
  return js_obj_to_alist(ar[0]);
});
define_libfunc("js-obj->alist", 1, 1, function(ar) {
  return js_obj_to_alist(ar[0]);
});

//
// timer, sleep
//
define_libfunc("timer", 2, 2, function(ar, intp){
  var proc = ar[0], sec = ar[1];
  assert_closure(proc);
  assert_real(sec);
  var intp2 = new Interpreter(intp);
  setTimeout(function(){ intp2.invoke_closure(proc); }, sec * 1000);
  return undef;
});
define_libfunc("set-timer!", 2, 2, function(ar, intp){
  var proc = ar[0], sec = ar[1];
  assert_closure(proc);
  assert_real(sec);
  var intp2 = new Interpreter(intp);
  return setInterval(function(){ intp2.invoke_closure(proc); }, sec * 1000);
});
define_libfunc("clear-timer!", 1, 1, function(ar){
  var timer_id = ar[0];
  clearInterval(timer_id);
  return undef;
});
define_libfunc("sleep", 1, 1, function(ar){
  var sec = ar[0];
  assert_real(sec);
  return new Pause(function(pause){
    setTimeout(function(){ pause.resume(nil); }, sec * 1000);
  });
});

//
// console
//
// (console-debug obj1 ...)
// (console-log obj1 ...)
// (console-info obj1 ...)
// (console-warn obj1 ...)
// (console-error obj1 ...)
//   Put objects to console, if window.console is defined.
//   Returns obj1.
//
// Example:
//     (some-func arg1 (console-debug arg2) arg3)
var define_console_func = function(name){
  define_libfunc("console-"+name, 1, null, function(ar){
    var con = window.console;
    if(con){
      var vals = _.map(ar, function(item){
        return inspect(item, {fallback: item});
      });

      con[name].apply(con, vals);
    }
    return ar[0];
  });
};
define_console_func("debug");
define_console_func("log");
define_console_func("info");
define_console_func("warn");
define_console_func("error");

