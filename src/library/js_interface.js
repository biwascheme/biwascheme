if( typeof(BiwaScheme)!='object' ) BiwaScheme={}; with(BiwaScheme) {

  //
  // interface to javascript
  //
  define_libfunc_raw("js-eval", 1, 1, function(ar){
    return eval(ar[0]);
  });
  define_libfunc_raw("js-ref", 2, 2, function(ar){
    assert_string(ar[1]);
    return ar[0][ar[1]]; //todo: first-class js function
  });
  define_libfunc("js-set!", 3, 3, function(ar){
    assert_string(ar[1]);
    ar[0][ar[1]] = ar[2];
    return BiwaScheme.undef;
  });

  // (js-call (js-eval "Math.pow") 2 4)
  define_libfunc_raw("js-call", 1, null, function(ar){
    var js_func = ar.shift();
    assert_function(js_func);

    var receiver = null;
    return js_func.apply(receiver, ar);
  });
  // (js-invoke (js-new "Date") "getTime")
  define_libfunc_raw("js-invoke", 2, null, function(ar){
    var js_obj = ar.shift();
    var func_name = ar.shift();
    assert_string(func_name);
    if(js_obj[func_name])
      return js_obj[func_name].apply(js_obj, ar);
    else
      throw new Error("js-invoke: function "+func_name+" is not defined");
  });

  // (js-new "Date" 2005 1 1)
  // (js-new "Draggable elem 'onEnd (lambda (drg) ...))
  //   If symbol is given, following arguments are converted to 
  //   an js object. If any of them is a scheme closure,
  //   it is converted to js function which invokes that closure.
  //
  define_libfunc("js-new", 1, null, function(ar, intp){
    // make js object from key-value pair
    var array_to_obj = function(ary){
      if((ary.length % 2) != 0)
        throw new Error("js-new: odd number of key-value pair");

      var obj = {};
      for(var i=0; i<ary.length; i+=2){
        var key = ary[i], value = ary[i+1];
        assert_symbol(key);
        if(value.closure_p === true)
          value = BiwaScheme.js_closure(value, intp);

        obj[key.name] = value;
      }
      return obj;
    };

    var ctor = ar.shift();
    assert_string(ctor);
    if(ar.length == 0){
      return eval("new " + ctor + "()");
    }
    else{
      // pack args to js object, if symbol appears
      var args = [];
      for(var i=0; i<ar.length; i++){
        if(ar[i] instanceof Symbol){
          args.push(array_to_obj(ar.slice(i)));
          break;
        }
        else{
          args.push(ar[i]);
        }
      }
      // construct js code to create new object
      var args_str = _.map(ar, function(value, i){
        return "args['" + i + "']";
      }).join(",");
      return eval("new " + ctor + "(" + args_str + ")");
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

  // (js-closure (lambda (event) ..))
  BiwaScheme.js_closure = function(proc, intp){
    var on_error = intp.on_error;
    return function(/*args*/){
      var intp = new Interpreter(on_error);
      return intp.invoke_closure(proc, _.toArray(arguments));
    };
  };
  define_libfunc("js-closure", 1, 1, function(ar, intp){
    assert_closure(ar[0]);
    return BiwaScheme.js_closure(ar[0], intp);
  });

  define_libfunc("js-null?", 1, 1, function(ar){
    return ar[0] === null;
  });

  define_libfunc("js-undefined?", 1, 1, function(ar){
    return ar[0] === undefined;
  });

  define_libfunc_raw("js-array-to-list", 1, 1, function(ar){
    return BiwaScheme.array_to_list(ar[0]);
  });

  define_libfunc_raw("list-to-js-array", 1, 1, function(ar){
    return ar[0].to_array();
  });

  //
  // timer, sleep
  //
  define_libfunc("timer", 2, 2, function(ar, intp){
    var proc = ar[0], sec = ar[1];
    assert_closure(proc);
    assert_real(sec);
    setTimeout(function(){ (new Interpreter(intp.on_error)).invoke_closure(proc); }, sec * 1000);
    return BiwaScheme.undef;
  });
  define_libfunc("set-timer!", 2, 2, function(ar, intp){
    var proc = ar[0], sec = ar[1];
    assert_closure(proc);
    assert_real(sec);
    return setInterval(function(){ (new Interpreter(intp.on_error)).invoke_closure(proc); }, sec * 1000);
  });
  define_libfunc("clear-timer!", 1, 1, function(ar){
    var timer_id = ar[0];
    clearInterval(timer_id);
    return BiwaScheme.undef;
  });
  define_libfunc("sleep", 1, 1, function(ar){
    var sec = ar[0];
    assert_real(sec);
    return new BiwaScheme.Pause(function(pause){
      setTimeout(function(){ pause.resume(nil); }, sec * 1000);
    });
  });

}
