///
/// infra.js - Basis for library functions
///

//
// define_*func - define library functions
//
BiwaScheme.check_arity = function(len, min, max){
  var fname = arguments.callee.caller
                ? arguments.callee.caller.fname 
                : "";
  if(len < min){
    if(max && max == min)
      throw new BiwaScheme.Error(fname+": wrong number of arguments (expected: "+min+" got: "+len+")");
    else
      throw new BiwaScheme.Error(fname+": too few arguments (at least: "+min+" got: "+len+")");
  }
  else if(max && max < len)
    throw new BiwaScheme.Error(fname+": too many arguments (at most: "+max+" got: "+len+")");
}
BiwaScheme.define_libfunc = function(fname, min, max, func, is_raw){
  var f = function(ar, intp){
    BiwaScheme.check_arity(ar.length, min, max);
    var result = func(ar, intp);
    if (is_raw) {
      return result;
    }
    else{
      if (result === undefined){
        throw new BiwaScheme.Bug("library function " + 
                                 "`" + fname + "'" +
                                 " returned JavaScript's undefined");
      }
      else if (result === null){
        throw new BiwaScheme.Bug("library function " +
                                 "`" + fname + "'" + 
                                 " returned JavaScript's null");
      }
      else {
        return result;
      }
    }
  };

  func["fname"] = fname; // for assert_*
  f["fname"]    = fname; // for check_arity
  f["inspect"] = function(){ return this.fname; }
  BiwaScheme.CoreEnv[fname] = f;
}
BiwaScheme.define_libfunc_raw = function(fname, min, max, func){
  BiwaScheme.define_libfunc(fname, min, max, func, true);
}
BiwaScheme.define_syntax = function(sname, func) {
  var s = new BiwaScheme.Syntax(sname, func);
  BiwaScheme.TopEnv[sname] = s;
}
BiwaScheme.define_scmfunc = function(fname, min, max, str){
  (new Interpreter).evaluate("(define "+fname+" "+str+"\n)");
}

//  define_scmfunc("map+", 2, null, 
//    "(lambda (proc ls) (if (null? ls) ls (cons (proc (car ls)) (map proc (cdr ls)))))");

//
// assertions - type checks
//
var make_assert = function(check){
  return function(/*args*/){
    var fname = arguments.callee.caller
                  ? arguments.callee.caller.fname 
                  : "";
    check.apply(this, [fname].concat($A(arguments)));
  }
}
var make_simple_assert = function(type, test){
  return make_assert(function(fname, obj){
    if(!test(obj)){
      throw new BiwaScheme.Error(fname + ": " +
                                 type + " required, but got " +
                                 BiwaScheme.to_write(obj));
    }
  })
}

var assert_number = make_simple_assert("number", function(obj){
  return typeof(obj) == 'number' || (obj instanceof BiwaScheme.Complex);
});

var assert_integer = make_simple_assert("integer", function(obj){
  return typeof(obj) == 'number' && (obj % 1 == 0)
});

var assert_real = make_simple_assert("real number", function(obj){
  return typeof(obj) == 'number';
});

var assert_symbol = make_simple_assert("symbol", function(obj){
  return obj instanceof BiwaScheme.Symbol;
});

var assert_string = make_simple_assert("string", function(obj){
  return typeof(obj) == 'string';
});

var assert_vector = make_simple_assert("vector", function(obj){
  return (obj instanceof Array) && (obj.closure_p !== true);
});

var assert_pair = make_simple_assert("pair", function(obj){
  return obj instanceof BiwaScheme.Pair;
});

var assert_char = make_simple_assert("character", function(obj){
  return obj instanceof BiwaScheme.Char;
});

var assert_port = make_simple_assert("port", function(obj){
  return obj instanceof BiwaScheme.Port;
});

var assert_date = make_simple_assert("date", function(obj){
  return obj instanceof Date;
});

var assert_hashtable = make_simple_assert("hashtable", function(obj){
  return obj instanceof BiwaScheme.Hashtable;
});

var assert_mutable_hashtable = make_simple_assert("hashtable", function(obj){
  return (obj instanceof BiwaScheme.Hashtable) && obj.mutable;
});

var assert_function = make_simple_assert("JavaScript function", function(obj){
  return (obj instanceof Function) || (typeof obj == 'function');
});

var assert_closure = make_simple_assert("scheme function", function(obj){
  return (obj instanceof Array) && (obj.closure_p === true);
});

var assert_applicable = make_simple_assert("scheme/js function", function(obj){
  return (obj instanceof Function) || (typeof obj == 'function') ||
         ((obj instanceof Array) && (obj.closure_p === true));
});

var assert_between = make_assert(function(fname, obj, from, to){
  if( typeof(obj) != 'number' || obj != Math.round(obj) ){
    throw new BiwaScheme.Error(fname + ": " +
                               "number required, but got " +
                               BiwaScheme.to_write(obj));
  }

  if( obj < from || to < obj ){
    throw new BiwaScheme.Error(fname + ": " + 
                               "number must be between " + 
                               from + " and " + to + ", but got " +
                               BiwaScheme.to_write(obj));
  }
});

var assert = make_assert(function(fname, test){
});


