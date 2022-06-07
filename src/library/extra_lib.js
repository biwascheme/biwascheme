import { escape } from "../deps/underscore-esm.js"
import { TopEnv, nil, undef } from "../header.js";
import { define_libfunc, alias_libfunc, define_syntax, define_scmfunc,
         assert_number, assert_integer, assert_real, assert_between, assert_string,
         assert_char, assert_symbol, assert_port, assert_pair, assert_list,
         assert_vector,
         assert_function, assert_closure, assert_procedure, assert_date } from "./infra.js"; 
import { to_write, to_display, inspect } from "../system/_writer.js"
import { Pair, List, array_to_list, deep_array_to_list } from "../system/pair.js"
import { BiwaSymbol, Sym, gensym } from "../system/symbol.js"
import Call from "../system/call.js"
import { Closure } from "../system/closure.js"
import Compiler from "../system/compiler.js"
import Console from "../system/console.js"
import { Bug } from "../system/error.js"
import Interpreter from "../system/interpreter.js"
import { Port } from "../system/port.js"
import Syntax from "../system/syntax.js"

define_libfunc("html-escape", 1, 1, function(ar){
  assert_string(ar[0]);
  return escape(ar[0]);
});
const inspect_objs = function(objs){
  return objs.map(inspect).join(", ");
};
define_libfunc("inspect", 1, null, function(ar){
  return inspect_objs(ar);
});
define_libfunc("inspect!", 1, null, function(ar){
  Console.puts(inspect_objs(ar));
  return undef;
});

//
// json
//
// json->sexp
// Array -> list
// Object -> alist
// (number, boolean, string, 
//
const json2sexp = function(json){
  switch(true){
  case typeof json === "number" ||
       typeof json === "string" ||
       json === true || json === false:
    return json;
  case Array.isArray(json):
    return array_to_list(json.map(json2sexp));
  case typeof(json) == "object":
    var ls = nil;
    for(key in json){
      ls = new Pair(new Pair(key, json2sexp(json[key])),
             ls);
    }
    return ls;
  default:
    throw new Error("json->sexp: detected invalid value for json: "+inspect(json));
  }
  throw new Bug("must not happen");
}
define_libfunc("json->sexp", 1, 1, function(ar){
  return json2sexp(ar[0]);
})

// (vector-push! v item1 item2 ...)
define_libfunc("vector-push!", 2, null, function(ar){
  assert_vector(ar[0]);
  for(var i=1; i<ar.length; i++){
    ar[0].push(ar[i]);
  }
  return ar[0];
});

//
//from Gauche
//

// (identity obj)
// Returns obj.
define_libfunc("identity", 1, 1, function(ar){
  return ar[0];
});

// (inc! i)
// = (begin (set! i (+ i 1)) i)
// Increments i (i.e., set i+1 to i).
define_syntax("inc!", function(x){
  var target = x.cdr.car;
  return List(Sym("begin"),
              List(Sym("set!"),
                   target, 
                   List(Sym("+"), target, 1)),
              target);
});

// (dec! i)
// = (begin (set! i (- i 1)) i)
// Decrements i (i.e., set i-1 to i).
define_syntax("dec!", function(x){
  var target = x.cdr.car;
  return List(Sym("begin"),
              List(Sym("set!"),
                   target, 
                   List(Sym("-"), target, 1)),
              target);
});

// string

define_libfunc("string-concat", 1, 1, function(ar){
  assert_list(ar[0]);
  return ar[0].to_array().join("");
})

define_libfunc("string-split", 2, 2, function(ar){
  assert_string(ar[0]);
  assert_string(ar[1]);
  return array_to_list(ar[0].split(ar[1]));
})

define_libfunc("string-join", 1, 2, function(ar){
  assert_list(ar[0]);
  var delim = ""
  if(ar[1]){
    assert_string(ar[1]);
    delim = ar[1];
  }
  return ar[0].to_array().join(delim);
})

// lists

define_libfunc("intersperse", 2, 2, function(ar){
  var item = ar[0], ls = ar[1];
  assert_list(ls);

  var ret = [];
  ls.to_array().reverse().forEach(function(x){
    ret.push(x);
    ret.push(item);
  });
  ret.pop();
  return array_to_list(ret);
});

define_libfunc("map-with-index", 2, null, function(ar){
  var proc = ar.shift(), lists = ar;
  lists.forEach(assert_list);

  var results = [], i = 0;
  return Call.multi_foreach(lists, {
    call: function(xs){ 
      var args = xs.map(function(x){ return x.car });
      args.unshift(i);
      i++;
      return new Call(proc, args);
    },
    result: function(res){ results.push(res); },
    finish: function(){ return array_to_list(results); }
  });
});

// loop

// (dotimes (variable limit result) body ...)
// Iterate with variable 0 to limit-1.
// ->
//    (do ((tlimit limit)
//         (variable 0 (+ variable 1)))
//        ((>= variable tlimit) result)
//      body ...)
define_syntax("dotimes", function(x){
  var spec = x.cdr.car,
      bodies = x.cdr.cdr;
  var variable = spec.car,
      limit = spec.cdr.car,
      result = spec.cdr.cdr.car;
  var tlimit = gensym();

  var do_vars = deep_array_to_list([[tlimit, limit],
                                    [variable, 0, [Sym("+"), variable, 1]]]);
  var do_check = deep_array_to_list([[Sym(">="), variable, tlimit], result]);

  return new Pair(Sym("do"),
           new Pair(do_vars,
             new Pair(do_check,
               bodies)));
});

// sorting (Obsolete: use list-sort, etc. instead of these.)

// utility function. takes a JS Array and a Scheme procedure,
// returns sorted array
var sort_with_comp = function(ary, proc, intp){
  return ary.sort(function(a, b){
      var intp2 = new Interpreter(intp);
      return intp2.invoke_closure(proc, [a, b]);
    });
};

define_libfunc("list-sort/comp", 1, 2, function(ar, intp){
  assert_procedure(ar[0]);
  assert_list(ar[1]);

  return array_to_list(sort_with_comp(ar[1].to_array(), ar[0], intp));
});
define_libfunc("vector-sort/comp", 1, 2, function(ar, intp){
  assert_procedure(ar[0]);
  assert_vector(ar[1]);

  return sort_with_comp([...ar[1]], ar[0], intp);
});
define_libfunc("vector-sort/comp!", 1, 2, function(ar, intp){
  assert_procedure(ar[0]);
  assert_vector(ar[1]);

  sort_with_comp(ar[1], ar[0], intp);
  return undef;
});

// macros

//(define-macro (foo x y) body ...)
//(define-macro foo lambda)

var rearrange_args = function (expected, given) {
  var args = [];
  var dotpos = (new Compiler).find_dot_pos(expected);
  if (dotpos == -1)
    args = given;
  else {
    for (var i = 0; i < dotpos; i++) {
      args[i] = given[i];
    }
    args[i] = array_to_list(given.slice(i));
  }
  return args;
}
define_syntax("define-macro", function(x){
  var head = x.cdr.car;
  var expected_args;
  if(head instanceof Pair){
    var name = head.car;
    expected_args = head.cdr;
    var body = x.cdr.cdr;
    var lambda = new Pair(Sym("lambda"),
                   new Pair(expected_args,
                     body))
  }
  else{
    var name = head;
    var lambda = x.cdr.cdr.car;
    expected_args = lambda.cdr.car;
  }

  //["close", <args>, <n>, <body>, <opecodes_next>, <dotpos>]
  var opc = Compiler.compile(lambda).il;
  if(opc[2] != 0)
    throw new Bug("you cannot use free variables in macro expander (or define-macro must be on toplevel)")
  var cls = new Closure(opc[3], [], -1, undefined);

  TopEnv[name.name] = new Syntax(name.name, function(sexp){
    var given_args = sexp.to_array();

    given_args.shift();
    
    var intp = new Interpreter();
    var args = rearrange_args(expected_args, given_args);
    var result = intp.invoke_closure(cls, args);
    return result;
  });

  return undef;
})

var macroexpand_1 = function(x){
  if(x instanceof Pair){
    // TODO: Should we check CoreEnv too?
    if(x.car instanceof BiwaSymbol && TopEnv[x.car.name] instanceof Syntax){
      var transformer = TopEnv[x.car.name];
      x = transformer.transform(x);
    }
    else
      throw new Error("macroexpand-1: `" + to_write(x) + "' is not a macro");
  }
  return x;
}
define_syntax("%macroexpand", function(x){
  var expanded = Compiler.expand(x.cdr.car);
  return List(Sym("quote"), expanded);
});
define_syntax("%macroexpand-1", function(x){
  var expanded = macroexpand_1(x.cdr.car);
  return List(Sym("quote"), expanded);
});

define_libfunc("macroexpand", 1, 1, function(ar){
  return Compiler.expand(ar[0]);
});
define_libfunc("macroexpand-1", 1, 1, function(ar){
  return macroexpand_1(ar[0]);
});

define_libfunc("gensym", 0, 1, function(ar){
  if (ar[0] == undefined) {
     return gensym();
   } else {
     assert_string(ar[0]);
     return gensym(ar[0]);
   }
});

// i/o

define_libfunc("print", 1, null, function(ar){
  ar.map(function(item){
    Console.puts(to_display(item), true);
  })
  Console.puts(""); //newline
  return undef;
})
define_libfunc("write-to-string", 1, 1, function(ar){
  return to_write(ar[0]);
});
define_libfunc("read-from-string", 1, 1, function(ar){
  assert_string(ar[0]);
  return Interpreter.read(ar[0]);
});
define_libfunc("port-closed?", 1, 1, function(ar){
  assert_port(ar[0]);
  return !(ar[0].is_open);
});
//define_libfunc("with-input-from-port", 2, 2, function(ar){
//define_libfunc("with-error-to-port", 2, 2, function(ar){
define_libfunc("with-output-to-port", 2, 2, function(ar){
  var port = ar[0], proc = ar[1];
  assert_port(port);
  assert_procedure(proc);

  var original_port = Port.current_output;
  Port.current_output = port

  return new Call(proc, [port], function(ar){
    port.close();
    Port.current_output = original_port;

    return ar[0];
  });
});

// syntax

define_syntax("let1", function(x){
  //(let1 vari expr body ...) 
  //=> ((lambda (var) body ...) expr)
  var vari = x.cdr.car; 
  var expr = x.cdr.cdr.car;
  var body = x.cdr.cdr.cdr;

  return new Pair(new Pair(Sym("lambda"),
                    new Pair(new Pair(vari, nil),
                      body)),
           new Pair(expr, nil));
})

//
// Regular Expression
//
var assert_regexp = function(obj, fname){
  if(!(obj instanceof RegExp))
    throw new Error(fname + ": regexp required, but got " + to_write(obj));
}

//Function: string->regexp string &keyword case-fold 
define_libfunc("string->regexp", 1, 1, function(ar){
  assert_string(ar[0], "string->regexp");
  return new RegExp(ar[0]); //todo: case-fold
})
//Function: regexp? obj 
define_libfunc("regexp?", 1, 1, function(ar){
  return (ar[0] instanceof RegExp);
})
//Function: regexp->string regexp 
define_libfunc("regexp->string", 1, 1, function(ar){
  assert_regexp(ar[0], "regexp->string");
  return ar[0].toString().slice(1, -1); //cut '/' 
})

define_libfunc("regexp-exec", 2, 2, function(ar){
  var rexp = ar[0];
  if(typeof ar[0] === "string"){
    rexp = new RegExp(ar[0]);
  }
  assert_regexp(rexp, "regexp-exec");
  assert_string(ar[1], "regexp-exec");
  var ret = rexp.exec(ar[1])
  return (ret === null) ? false : array_to_list(ret);
})

//  //Function: rxmatch regexp string 
//  define_libfunc("rxmatch", 1, 1, function(ar){
//    assert_regexp(ar[0], "rxmatch");
//    assert_string(ar[1], "rxmatch");
//    return ar[0].match(ar[1]);
//  });
//Function: rxmatch-start match &optional (i 0) 
//Function: rxmatch-end match &optional (i 0) 
//Function: rxmatch-substring match &optional (i 0) 
//Function: rxmatch-num-matches match   
//Function: rxmatch-after match &optional (i 0) 
//Function: rxmatch-before match &optional (i 0) 
//Generic application: regmatch &optional index 
//Generic application: regmatch 'before &optional index 
//Generic application: regmatch 'after &optional index 
//Function: regexp-replace regexp string substitution 

// regexp-replace-all regexp string substitution 
define_libfunc("regexp-replace-all", 3, 3, function(ar){
  var pat = ar[0];
  if(typeof pat === "string"){
    var rexp = new RegExp(pat, "g")
  }
  else{
    assert_regexp(pat);
    var rexp = new RegExp(pat.source, "g")
  }
  assert_string(ar[1]);
  assert_string(ar[2]);
  return ar[1].replace(rexp, ar[2])
})
//Function: regexp-replace* string rx1 sub1 rx2 sub2 ... 
//Function: regexp-replace-all* string rx1 sub1 rx2 sub2 ... 
//Function: regexp-quote string 
//Macro: rxmatch-let match-expr (var ...) form ... 
//Macro: rxmatch-if match-expr (var ...) then-form else-form 
//Macro: rxmatch-cond clause ... 
//Macro: rxmatch-case string-expr clause ... 
