import { TopEnv, CoreEnv, nil, undef, max_trace_size } from "../header.js"
import { isSymbol } from "./_types.js"
import { to_write, inspect } from "./_writer.js"
import Call from "./call.js"
import { Closure, isClosure } from "./closure.js"
import Compiler from "./compiler.js"
import { BiwaError, Bug } from "./error.js"
import { Pair, List } from "./pair.js"
import Parser from "./parser.js"
import Pause from "./pause.js"
import { eof } from "./port.js"
import { Sym } from "./symbol.js"

///
/// Interpreter
///

class Interpreter {
  // new Interpreter()
  // new Interpreter(lastInterpreter)
  // new Interpreter(errorHandler)
  // new Interpreter(lastInterpreter, errorHandler)
  constructor(){
    var last_interpreter = null;
    var on_error = null;
    if (arguments.length == 2) {
      last_interpreter = arguments[0];
      on_error = arguments[1];
    } else if (arguments.length == 1 && arguments[0] instanceof Interpreter) {
      last_interpreter = arguments[0];
    } else if (arguments.length == 1 && typeof(arguments[0]) == "function") {
      on_error = arguments[0];
    }

    // Interpreter stack
    this.stack = [];
    // JS function to handle error
    this.on_error = on_error || (last_interpreter ? last_interpreter.on_error : function(e){});
    // JS function to handle result
    this.after_evaluate = function(){};

    // (Variables for stack trace)
    // Name of the last variable read by refer-xx
    this.last_refer = last_interpreter ? last_interpreter.last_refer : null;
    // Call stack (array of last_refer)
    this.call_stack = last_interpreter ? [...last_interpreter.call_stack] : [];
    // Counts number of tail calls (= how many times should we pop call_stack
    // in op_return)
    this.tco_counter = [];
    // Maximum length of call_stack
    // (Note: we should cap call_stack for inifinite loop with recursion)
    this.max_trace_size = last_interpreter ? last_interpreter.max_trace_size : max_trace_size;

    // dynamic-wind
    this.current_dynamic_winder = Interpreter.DynamicWind.ROOT;
  }

  inspect(){
    return [
      "#<Interpreter: stack size=>",
      this.stack.length, " ",
      "after_evaluate=",
      inspect(this.after_evaluate),
      ">"
    ].join("");
  }

  // private
  push(x, s){
    this.stack[s] = x;
    return s+1;
  }

  // private
  //s: depth of stack to save
  //ret: saved(copied) stack 
  save_stack(s){
    var v = [];
    for(var i=0; i<s; i++){
      v[i] = this.stack[i];
    }
    return { stack: v, last_refer: this.last_refer, call_stack: [...this.call_stack], tco_counter: [...this.tco_counter] };
  }

  // private
  //v: stack array to restore
  //ret: lenght of restored stack
  restore_stack(stuff){
    const v = stuff.stack;
    const s = v.length;
    for(var i=0; i<s; i++){
      this.stack[i] = v[i];
    }
    this.last_refer = stuff.last_refer;
    this.call_stack = [...stuff.call_stack];
    this.tco_counter = [...stuff.tco_counter];
    return s;
  }

  // private
  //s: depth of stack to save
  //n: number of args(for outer lambda) to remove (= 0 unless tail position)
  //ret: closure array
  capture_continuation(s, n){
    // note: implementation of this function for final version doesn't exist in 3imp.pdf..
    var ss = this.push(n, s);
    return this.closure(["nuate1", this.save_stack(ss), this.current_dynamic_winder],
                        1,     //arity
                        0,     //n (number of frees)
                        null,  //s (stack position to get frees)
                        -1);   // dotpos
  }

  // private
  // shift stack 
  // n: number of items to skip (from stack top)
  // m: number of items to shift
  // s: stack pointer (= index of stack top + 1)
  shift_args(n, m, s){
    for(var i = n; i >= 0; i--){
      this.index_set(s, i+m+1, this.index(s, i));
    }
    return s-m-1;
  }

  index(s, i){
    return this.stack[(s-1)-i];
  }

  // private
  index_set(s, i, v){
    this.stack[(s-1)-i] = v;
  }

  // private
  closure(body, n_args, n, s, dotpos){
    const freevars = [];
    for(var i=0; i<n; i++) {
      freevars[i] = this.index(s, i);
    }
    const expected_args = dotpos == -1 ? n_args : undefined;
    return new Closure(body, freevars, dotpos, expected_args);
  }

  // private
  run_dump_hook(a, x, f, c, s) {
    var dumper;
    var state;


    if (this.dumper) {
      dumper = this.dumper;
    }
    else if (Interpreter.dumper) {
      dumper = Interpreter.dumper;
    }
    else
      return;

    if (dumper) {
      state = {"a":a,
               "f":f,
               "c":c,
               "s":s,
               "x":x,
               "stack":this.stack};
      dumper.dump(state);
    }
  }

  // private
  // a: arbitary object (temporary register)
  // x: opecode
  // f: integer
  // c: BiwaScheme.Closure
  // s: integer
  _execute(a, x, f, c, s){
    var ret = null;
    //Console.puts("executing "+x[0]);
    
    while(true){ //x[0] != "halt"){
      this.run_dump_hook(a, x, f, c, s);

      switch(x[0]){
      case "halt":
        return a;
      case "refer-local":
        var n=x[1], x=x[2];
        a = this.index(f, n+1);
        this.last_refer = "(anon)";
        break;
      case "refer-free":
        var n=x[1], x=x[2];
        a = c.freevars[n];
        this.last_refer = "(anon)";
        break;
      case "refer-global":
        var sym=x[1], x=x[2];
        if(TopEnv.hasOwnProperty(sym))
          var val = TopEnv[sym];
        else if(CoreEnv.hasOwnProperty(sym))
          var val = CoreEnv[sym];
        else
          throw new BiwaError("execute: unbound symbol: "+inspect(sym));

        a = val;
        this.last_refer = sym || "(anon)";
        break;
      case "indirect":
        var x=x[1];
        a = a[0]; //unboxing
        break;
      case "constant":
        var obj=x[1], x=x[2];
        a = obj;
        this.last_refer = "(anon)";
        break;
      case "close":
        var ox=x;
        var v=ox[1], n=ox[2], body=ox[3], x=ox[4], dotpos=ox[5];
        a = this.closure(body, v, n, s, dotpos);
        s -= n;
        break;
      case "box":
        var n=x[1], x=x[2];
        this.index_set(s, n+1, [this.index(s, n+1)]); //boxing
        break;
      case "test":
        var thenc=x[1], elsec=x[2];
        x = ((a!==false) ? thenc : elsec);
        break;
      case "assign-global":
        var name=x[1], x=x[2];
        if(!TopEnv.hasOwnProperty(name) &&
           !CoreEnv.hasOwnProperty(name))
          throw new BiwaError("global variable '"+name+"' is not defined");
        
        TopEnv[name] = a;
        a = undef;
        break;
      case "assign-local":
        var n=x[1], x=x[2];
        var box = this.index(f, n+1);
        box[0] = a;
        a = undef;
        break;
      case "assign-free":
        var n=x[1], x=x[2];
        var box = c.freevars[n];
        box[0] = a;
        a = undef;
        break;
      case "conti":
        var n=x[1], x=x[2];
        a = this.capture_continuation(s, n);
        break;
      case "nuate1":
        var stack=x[1], to=x[2];
        var from = this.current_dynamic_winder;
        var winders = Interpreter.DynamicWind.listWinders(from, to);
        x = Interpreter.DynamicWind.joinWinders(winders,
          ["refer-local", 0, ["nuate2", stack]]
        )
        break;
      case "nuate2":
        var stack=x[1], x=["return"];
        s = this.restore_stack(stack);
        break;
      case "frame":
        var ret = x[2];
        x = x[1];
        s = this.push(ret, this.push(f, this.push(c, s)));
        this.tco_counter[this.tco_counter.length] = 0;
        break;
      case "argument":
        var x=x[1];
        s = this.push(a, s);
        break;
      case "shift":
        var n=x[1], x=x[2];

        // the number of arguments in the last call
        var n_args = this.index(s, n+1);  

        s = this.shift_args(n, n_args, s);
        break;
      case "tco_hinted_apply": // just like a regular apply, except we need to trace the # of TCO calls so we can generate a stacktrace
        this.tco_counter[this.tco_counter.length - 1]++;
        x = ["apply"].concat(x.slice(1));
        break;
      case "apply": //extended: n_args as second argument
        var func = a; //, n_args = x[1];

        // Save stack trace
        this.call_stack.push(this.last_refer);
        if (this.call_stack.length > this.max_trace_size) {
          // Remove old memory if it grows too long
          // Note: this simple way may be inconvenient (e.g. no trace
          // will be shown when an error occurred right after returning
          // from a large function)
          this.call_stack.shift();
        }

        // the number of arguments in the last call is
        // pushed to the stack.
        var n_args = this.index(s, 0);
        if(isClosure(func)){
          a = func;
          x = func.body

          // The position of dot in the parameter list.
          const dotpos = func.dotpos;
          if (dotpos >= 0) {
            // The dot is found
            // ----------------
            // => Process the &rest args: packing the rest args into a list.
            var ls = nil;
            for (var i=n_args; --i>=dotpos; ) {
              ls = new Pair(this.index(s, i+1), ls);
            }
            if (dotpos >= n_args) {
              // No rest argument is passed to this closure.
              // However, the closure expects the caller passes the rest argument.
              // In such case this VM prepares an empty list as the rest argument.
              // --------------------------------------------------------------
              // => We extend the stack to put the empty list.
              for(var i = 0; i < n_args+1; i++){
                this.index_set(s, i-1, this.index(s, i));
              }
              s++;
              // => Update the number of arguments
              this.index_set(s, 0, this.index(s, 0) + 1);  
            }
            this.index_set(s, dotpos+1, ls);
          }
          else {
            // the dot is not found
            // --------------------
            // => Verify that number of arguments = expected number of arguments
            // (if the closure knows how many it wants)
            if(func.expected_args !== undefined && n_args != func.expected_args) {
              var errMsg = "Function call error: got " + n_args + " but wanted " + func.expected_args;
              throw new BiwaError(errMsg);
            }
          }
          f = s;
          c = func;
        }
        else if(func instanceof Function){ // Apply JavaScript function
          // load arguments from stack
          var args = [];
          for(var i=0; i<n_args; i++) 
            args.push(this.index(s, i+1));

          // invoke the function
          var result = func(args, this);

          if(result instanceof Pause){
            // it requested the interpreter to suspend
            var pause = result;
            pause.set_state(this, ["return"], f, c, s);
            pause.ready();
            return pause;
          }
          else if(result instanceof Call){
            // it requested the interpreter to call a scheme closure

            //   [frame,
            //     [constant... (args)
            //     [constant, proc
            //     [apply]]]]
            //   [frame,
            //     [constant, after
            //     [apply 1]]]]
            //   x
            var call_after = ["frame",
                               ["argument",
                               ["constant", 1,
                               ["argument",
                               ["constant", result.after,
                               ["apply"]]]]],
                             ["return"]];
            var call_proc = ["constant", result.args.length,
                            ["argument",
                            ["constant", result.proc, 
                            ["apply", result.args.length]]]];
            var push_args = result.args.reduce(function(opc, arg){
              // (foo 1 2) => first push 2, then 1
              //   [constant 2 ... [constant 1 ... ]
              return ["constant", arg, 
                     ["argument",
                     opc]];
            }, call_proc);
            x = ["frame",
                  push_args,
                call_after]
          }
          else{
            // the JavaScript function returned a normal value
            a = result;
            x = ["return"];
          }
        }
        else{
          // unknown function type
          throw new BiwaError(inspect(func) + " is not a function");
        }
        break;
      case "return":
        // Pop stack frame
        var n=this.index(s, 0);
        var ss=s-n;
        x = this.index(ss, 1),
        f = this.index(ss, 2),
        c = this.index(ss, 3),
        s = ss-3-1;

        // Pop stack trace (> 1 times if tail calls are done)
        var n_pops = 1 + this.tco_counter[this.tco_counter.length - 1];
        this.call_stack.splice(-n_pops);
        this.tco_counter.pop();
        break;
      default:
        throw new Bug("unknown opecode type: "+x[0]);
      }
    }

//      if(ret === null)
//        throw new Bug("interpreter exited in unusual way");
//      else
//        return ret;
    return a
  }

  // Compile and evaluate Scheme program
  evaluate(str, after_evaluate){
    this.call_stack = [];
    this.parser = new Parser(str);
    this.compiler = new Compiler();
    if(after_evaluate) 
      this.after_evaluate = after_evaluate;

    //Console.puts("executing: " + str);
     
    this.is_top = true;
    this.file_stack = [];

    try{
      return this.resume(false);
    }
    catch(e){
      e.message = e.message + " [" + this.call_stack.join(", ") + "]";
      return this.on_error(e);
    }
  }

  // Resume evaluation
  // (internally used from Interpreter#execute and Pause#resume)
  resume(is_resume, a, x, f, c, s){
    var ret = undef;

    for(;;){
      if(is_resume){
        ret = this._execute(a, x, f, c, s);
        is_resume = false;
      }
      else{
        if(!this.parser) break; // adhoc: when Pause is used via invoke_closure
        var expr = this.parser.getObject();
        if(expr === Parser.EOS) break;

        // expand
        expr = Compiler.expand(expr);

        // compile
        const vmcode = this.compiler.run(expr);

        // execute
        ret = this._execute(expr, vmcode.il, 0, [], 0);
      }

      if(ret instanceof Pause){ //suspend evaluation
        return ret;
      }
    }

    // finished executing all forms
    this.after_evaluate(ret);
    return ret;
  }

  // Invoke a scheme closure
  invoke_closure(closure, args){
    args || (args = []);
    var n_args  = args.length;

    var x = ["constant", n_args, ["argument", ["constant", closure, ["apply"]]]]
    for(var i=0; i<n_args; i++)
      x = ["constant", args[i], ["argument", x]]

    return this._execute(closure, ["frame", x, ["halt"]], 0, closure, 0);
  }

  // only compiling (for debug use only)
  compile(str){
    var obj = Interpreter.read(str);
    var opc = Compiler.compile(obj);
    return opc;
  }

  // before, after: Scheme closure
  push_dynamic_winder(before, after) {
    this.current_dynamic_winder =
      new Interpreter.DynamicWind(this.current_dynamic_winder, before, after);
  }

  pop_dynamic_winder(before, after) {
    this.current_dynamic_winder = this.current_dynamic_winder.parent;
  }
}

// Take a string and returns an expression.
Interpreter.read = function(str){
  var parser = new Parser(str);
  var r      = parser.getObject();
  return (r == Parser.EOS)? eof: r;
};

Interpreter.expand = function(){ throw "Interpreter.expand is moved to Compiler.expand" };

//
// dynamic-wind
//

Interpreter.DynamicWind = class {
  constructor(parent, before, after) {
    // Parent `DynamicWind` obj
    this.parent = parent;
    // "before" winder (Scheme closure)
    this.before = before;
    // "after" winder (Scheme closure)
    this.after = after;
  }
};

// A special value indicates the root of the winders
// (the string is just for debugging purpose.)
Interpreter.DynamicWind.ROOT = {_: "this is ROOT."};

// Return the list of winders to call
Interpreter.DynamicWind.listWinders = function(from, to) {
  // List winders from `from` to `ROOT`
  var fromStack = [from];
  while (from !== Interpreter.DynamicWind.ROOT) {
    from = from.parent;
    fromStack.push(from);
  }

  // List winders from `to` to `ROOT` and find the common one
  var toStack = [];
  var common;
  while (true) {
    var matched = fromStack.find(function(item) { return item === to });
    if (matched) {
      common = matched;
      break;
    }
    toStack.push(to);
    to = to.parent;
  }

  // List `after`s to call
  var ret = [];
  for (var i=0; i<fromStack.length; i++) {
    if (fromStack[i] === common) break;
    ret.push(fromStack[i].after);
  }

  // List `before`s to call
  toStack.reverse();
  toStack.forEach(function(item) {
    ret.push(item.before);
  });

  return ret;
};

// Return an opecode to run all the winders
Interpreter.DynamicWind.joinWinders = function(winders, x) {
  return winders.reduceRight(function(acc, winder) {
    return ["frame",
             ["constant", 0,
             ["argument",
             ["constant", winder,
             ["apply"]]]],
           acc];
  }, x);
}

export default Interpreter;
