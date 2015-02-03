///
/// Interpreter
///

BiwaScheme.Interpreter = BiwaScheme.Class.create({
  // new BiwaScheme.Interpreter()
  // new BiwaScheme.Interpreter(lastInterpreter)
  // new BiwaScheme.Interpreter(errorHandler)
  // new BiwaScheme.Interpreter(lastInterpreter, errorHandler)
  initialize: function(){
    var last_interpreter = null;
    var on_error = null;
    if (arguments.length == 2) {
      last_interpreter = arguments[0];
      on_error = arguments[1];
    } else if (arguments.length == 1 && arguments[0] instanceof BiwaScheme.Interpreter) {
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
    this.call_stack = last_interpreter ? _.clone(last_interpreter.call_stack) : [];
    // Counts number of tail calls (= how many times should we pop call_stack
    // in op_return)
    this.tco_counter = [];
    // Maximum length of call_stack
    // (Note: we should cap call_stack for inifinite loop with recursion)
    this.max_trace_size = last_interpreter ? last_interpreter.max_trace_size : BiwaScheme.max_trace_size;
  },

  inspect: function(){
    return [
      "#<Interpreter: stack size=>",
      this.stack.length, " ",
      "after_evaluate=",
      BiwaScheme.inspect(this.after_evaluate),
      ">"
    ].join("");
  },

  push: function(x, s){
    this.stack[s] = x;
    return s+1;
  },

  //s: depth of stack to save
  //ret: saved(copied) stack 
  save_stack: function(s){
    var v = [];
    for(var i=0; i<s; i++){
      v[i] = this.stack[i];
    }
    return { stack: v, last_refer: this.last_refer, call_stack: _.clone(this.call_stack), tco_counter: _.clone(this.tco_counter) };
  },

  //v: stack array to restore
  //ret: lenght of restored stack
  restore_stack: function(stuff){
    v = stuff.stack;
    var s = v.length;
    for(var i=0; i<s; i++){
      this.stack[i] = v[i];
    }
    this.last_refer = stuff.last_refer;
    this.call_stack = _.clone(stuff.call_stack);
    this.tco_counter = _.clone(stuff.tco_counter);
    return s;
  },

  //s: depth of stack to save
  //n: number of args(for outer lambda) to remove (= 0 unless tail position)
  //ret: closure array
  continuation: function(s, n){
    // note: implementation of this function for final version doesn't exist in 3imp.pdf..
    var ss = this.push(n, s);
    return this.closure(["refer-local", 0,
                          ["nuate", this.save_stack(ss), 
                          ["return"]]], 
                        0,     //n (number of frees)
                        null,  //s (stack position to get frees)
                        -1);   // dotpos
  },

  // shift stack 
  // n: number of items to skip (from stack top)
  // m: number of items to shift
  // s: stack pointer (= index of stack top + 1)
  shift_args: function(n, m, s){
    for(var i = n-1; i >= -1; i--){
      this.index_set(s, i+m+1, this.index(s, i));
    }
    return s-m-1;
  },

  index: function(s, i){
    return this.stack[s-i-2];
  },

  index_set: function(s, i, v){
    this.stack[s-i-2] = v;
  },

  //ret: [body, stack[s-1], stack[s-2], .., stack[s-n], dotpos]
  closure: function(body, n, s, dotpos){
    var v = []; //(make-vector n+1+1)
    v[0] = body;
    for(var i=0; i<n; i++)
      v[i+1] = this.index(s, i-1);
    v[n+1] = dotpos;

    v.closure_p = true;

    return v;
  },

  execute: function(a, x, f, c, s){
    var ret = null;
    try{
      ret = this._execute(a, x, f, c, s);
    }
    catch(e){
      e.message = e.message + " [" + this.call_stack.join(", ") + "]";
      var state = {a:a, x:x, f:f, c:c, s:s, stack:this.stack};
      return this.on_error(e, state);
    }
    return ret;
  },

  run_dump_hook: function(a, x, f, c, s) {
    var dumper;
    var state;


    if (this.dumper) {
      dumper = this.dumper;
    }
    else if (BiwaScheme.Interpreter.dumper) {
      dumper = BiwaScheme.Interpreter.dumper;
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
  },

  _execute: function(a, x, f, c, s){
    var ret = null;
    //puts("executing "+x[0]);
    
    while(true){ //x[0] != "halt"){

      this.run_dump_hook(a, x, f, c, s);

      switch(x[0]){
      case "halt":
        return a;
      case "refer-local":
        var n=x[1], x=x[2];
        a = this.index(f, n);
        this.last_refer = "(anon)";
        break;
      case "refer-free":
        var n=x[1], x=x[2];
        a = c[n+1];
        this.last_refer = "(anon)";
        break;
      case "refer-global":
        var sym=x[1], x=x[2];
        if(BiwaScheme.TopEnv.hasOwnProperty(sym))
          var val = BiwaScheme.TopEnv[sym];
        else if(BiwaScheme.CoreEnv.hasOwnProperty(sym))
          var val = BiwaScheme.CoreEnv[sym];
        else
          throw new BiwaScheme.Error("execute: unbound symbol: "+BiwaScheme.inspect(sym));

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
        var n=ox[1], body=ox[2], x=ox[3], dotpos=ox[4];
        a = this.closure(body, n, s, dotpos);
        s -= n;
        break;
      case "box":
        var n=x[1], x=x[2];
        this.index_set(s, n, [this.index(s, n)]); //boxing
        break;
      case "test":
        var thenc=x[1], elsec=x[2];
        x = ((a!==false) ? thenc : elsec);
        break;
      case "assign-global":
        var name=x[1], x=x[2];
        if(!BiwaScheme.TopEnv.hasOwnProperty(name) &&
           !BiwaScheme.CoreEnv.hasOwnProperty(name))
          throw new BiwaScheme.Error("global variable '"+name+"' is not defined");
        
        BiwaScheme.TopEnv[name] = a;
        a = BiwaScheme.undef;
        break;
      case "assign-local":
        var n=x[1], x=x[2];
        var box = this.index(f, n);
        box[0] = a;
        a = BiwaScheme.undef;
        break;
      case "assign-free":
        var n=x[1], x=x[2];
        var box = c[n+1];
        box[0] = a;
        a = BiwaScheme.undef;
        break;
      case "conti":
        var n=x[1], x=x[2];
        a = this.continuation(s, n);
        break;
      case "nuate":
        var stack=x[1], x=x[2];
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
        var n_args = this.index(s, n);  

        s = this.shift_args(n, n_args, s);
        break;
      case "tco_hinted_apply": // just like a regular apply, except we need to trace the # of TCO calls so we can generate a stacktrace
        this.tco_counter[this.tco_counter.length - 1]++;
        x = ["apply"].concat(_.rest(x));
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
        var n_args = this.index(s, -1);
        if(func instanceof Array){ //closure
          a = func;
          x = func[0];

          // The position of dot in the parameter list.
          var dotpos = func[func.length-1];

          if (dotpos >= 0) {
            // The dot is found
            // ----------------
            // => Process the &rest args: packing the rest args into a list.
            var ls = BiwaScheme.nil;
            for (var i=n_args; --i>=dotpos; ) {
              ls = new BiwaScheme.Pair(this.index(s, i), ls);
            }
            if (dotpos >= n_args) {
              // No rest argument is passed to this closure.
              // However, the closure expects the caller passes the rest argument.
              // In such case this VM prepares an empty list as the rest argument.
              // --------------------------------------------------------------
              // => We extend the stack to put the empty list.
              for(var i = -1; i < n_args; i++){
                this.index_set(s, i-1, this.index(s, i));
              }
              s++;
              // => Update the number of arguments
              this.index_set(s, -1, this.index(s, -1) + 1);  
            }
            this.index_set(s, dotpos, ls);
          }
          f = s;
          c = a;
        }
        else if(func instanceof Function){ // Apply JavaScript function
          // load arguments from stack
          var args = [];
          for(var i=0; i<n_args; i++) 
            args.push(this.index(s, i));

          // invoke the function
          var result = func(args, this);

          if(result instanceof BiwaScheme.Pause){
            // it requested the interpreter to suspend
            var pause = result;
            pause.set_state(this, ["return"], f, c, s);
            pause.ready();
            return pause;
          }
          else if(result instanceof BiwaScheme.Call){
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
            var push_args = _.inject(result.args, function(opc, arg){
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
          throw new BiwaScheme.Error(BiwaScheme.inspect(func) + " is not a function");
        }
        break;
      case "return":
        // Pop stack frame
        var n=this.index(s, -1);
        var ss=s-n;
        x = this.index(ss, 0),
        f = this.index(ss, 1),
        c = this.index(ss, 2),
        s = ss-3-1;

        // Pop stack trace (> 1 times if tail calls are done)
        var n_pops = 1 + this.tco_counter[this.tco_counter.length - 1];
        this.call_stack.splice(-n_pops);
        this.tco_counter.pop();
        break;
      default:
        throw new BiwaScheme.Bug("unknown opecode type: "+x[0]);
      }
    }

//      if(ret === null)
//        throw new BiwaScheme.Bug("interpreter exited in unusual way");
//      else
//        return ret;
    return a
  },

  evaluate: function(str, after_evaluate){
    this.parser = new BiwaScheme.Parser(str);
    this.compiler = new BiwaScheme.Compiler();
    if(after_evaluate) 
      this.after_evaluate = after_evaluate;

    if(BiwaScheme.Debug) puts("executing: " + str);
     
    this.is_top = true;
    this.file_stack = [];

    try{
      return this.resume(false);
    }
    catch(ex){
      return this.on_error(ex);
    }
  },

  resume: function(is_resume, a, x, f, c, s){
    var ret = BiwaScheme.undef;

    for(;;){
      if(is_resume){
        ret = this.execute(a, x, f, c, s);
        is_resume = false;
      }
      else{
        if(!this.parser) break; // adhoc: when Pause is used via invoke_closure
        var expr = this.parser.getObject();
        if(expr === BiwaScheme.Parser.EOS) break;

        // expand
        expr = BiwaScheme.Interpreter.expand(expr);

        // compile
        var opc = this.compiler.run(expr);
        //if(BiwaScheme.Debug) p(opc);

        // execute
        ret = this.execute(expr, opc, 0, [], 0);
      }

      if(ret instanceof BiwaScheme.Pause){ //suspend evaluation
        return ret;
      }
    }

    // finished executing all forms
    this.after_evaluate(ret);
    return ret;
  },

  invoke_closure: function(closure, args){
    args || (args = []);
    var n_args  = args.length;

    var x = ["constant", n_args, ["argument", ["constant", closure, ["apply"]]]]
    for(var i=0; i<n_args; i++)
      x = ["constant", args[i], ["argument", x]]

    return this.execute(closure, ["frame", x, ["halt"]], 0, closure, 0);
  },

  // only compiling (for debug use only)
  compile: function(str){
    var obj = BiwaScheme.Interpreter.read(str);
    var opc = BiwaScheme.Compiler.compile(obj);
    return opc;
  }
});

// Take a string and returns an expression.
BiwaScheme.Interpreter.read = function(str){
  var parser = new BiwaScheme.Parser(str);
  var r      = parser.getObject();
  return (r == BiwaScheme.Parser.EOS)? BiwaScheme.eof: r;
};

// Expand macro calls in a expression recursively.
//
// x - expression
// flag - used internally. do not specify this
//
// @throws {BiwaScheme.Error} when x has syntax error
BiwaScheme.Interpreter.expand = function(x, flag/*optional*/){
  var expand = BiwaScheme.Interpreter.expand;
  flag || (flag = {})
  var ret = null;

  if(x instanceof BiwaScheme.Pair){
    switch(x.car){
    case BiwaScheme.Sym("define"):
      var left = x.cdr.car, exp = x.cdr.cdr;
      ret = new BiwaScheme.Pair(BiwaScheme.Sym("define"),
              new BiwaScheme.Pair(left, expand(exp, flag)));
      break;
    case BiwaScheme.Sym("begin"):
      ret = new BiwaScheme.Pair(BiwaScheme.Sym("begin"), expand(x.cdr, flag));
      break;
    case BiwaScheme.Sym("quote"):
      ret = x;
      break;
    case BiwaScheme.Sym("lambda"):
      var vars=x.cdr.car, body=x.cdr.cdr;
      ret = new BiwaScheme.Pair(BiwaScheme.Sym("lambda"),
              new BiwaScheme.Pair(vars, expand(body, flag)));
      break;
    case BiwaScheme.Sym("if"):
      var testc=x.second(), thenc=x.third(), elsec=x.fourth();
      if (elsec == BiwaScheme.inner_of_nil){
        elsec = BiwaScheme.undef;
      }
      ret = BiwaScheme.List(BiwaScheme.Sym("if"),
                            expand(testc, flag),
                            expand(thenc, flag),
                            expand(elsec, flag));
      break;
    case BiwaScheme.Sym("set!"):
      var v=x.second(), x=x.third();
      ret = BiwaScheme.List(BiwaScheme.Sym("set!"), v, expand(x, flag));
      break;
    case BiwaScheme.Sym("call-with-current-continuation"): 
    case BiwaScheme.Sym("call/cc"): 
      var x=x.second();
      ret = BiwaScheme.List(BiwaScheme.Sym("call/cc"), expand(x, flag));
      break;
    default: //apply
      var transformer = null;
      if(BiwaScheme.isSymbol(x.car)){
        if(BiwaScheme.TopEnv[x.car.name] instanceof BiwaScheme.Syntax)
          transformer = BiwaScheme.TopEnv[x.car.name];
        else if(BiwaScheme.CoreEnv[x.car.name] instanceof BiwaScheme.Syntax)
          transformer = BiwaScheme.CoreEnv[x.car.name];
      }

      if(transformer){
        flag["modified"] = true;
        ret = transformer.transform(x);

//            // Debug
//            var before = BiwaScheme.to_write(x);
//            var after = BiwaScheme.to_write(ret);
//            if(before != after){
//              console.log("before: " + before)
//              console.log("expand: " + after)
//            }

        var fl;
        for(;;){
          ret = expand(ret, fl={});
          if(!fl["modified"]) 
            break;
        }
      }
      else{
        var expanded_car = expand(x.car, flag);
        var expanded_cdr;
        if(!(x.cdr instanceof BiwaScheme.Pair) && (x.cdr !== BiwaScheme.nil)){
          throw new Error("proper list required for function application "+
                          "or macro use: "+BiwaScheme.to_write(x));
        }
        expanded_cdr = BiwaScheme.array_to_list(
                         _.map(x.cdr.to_array(),
                               function(item){ return expand(item, flag); }));
        ret = new BiwaScheme.Pair(expanded_car, expanded_cdr);
      }
    }
  }
  else{
    ret = x;
  }
  return ret;
};
