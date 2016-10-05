///
/// Compiler
///
/// Note: macro expansion is done by Intepreter#expand

BiwaScheme.Compiler = BiwaScheme.Class.create({
  initialize: function(){
  },

  is_tail: function(x){
    return (x[0] == "return");
  },

  //free: set
  //e: env(= [locals, frees])
  //next: opc
  //ret: opc["refer_*", n, ["argument", 
  //          ["refer_*", n, ... ["argument", next]
  collect_free: function(free, e, next){
    var vars = free;
    var opc = next;
    var arr = vars.arr;
    for(var i=0; i<arr.length; i++){
      opc = this.compile_refer(arr[i], e, ["argument", opc]);
    }
    //Console.puts("collect_free "+free.inspect()+" / "+e.inspect()+" => "+opc.inspect());
    return opc;
  },

  //x: Symbol
  //e: env [set of locals, set of frees]
  //ret: opc
  compile_refer: function(x, e, next){
    return this.compile_lookup(x, e,
             function(n){ return ["refer-local", n, next] },
             function(n){ return ["refer-free",  n, next] },
             function(sym){ return ["refer-global", sym, next] });
  },

  compile_lookup: function(x, e, return_local, return_free, return_global){
    var locals = e[0], free = e[1];
    if((n = locals.index(x)) != null){
      //Console.puts("compile_refer:"+x.inspect()+" in "+e.inspect()+" results refer-local "+n);
      return return_local(n);
    }
    else if((n = free.index(x)) != null){
      //Console.puts("compile_refer:"+x.inspect()+" in "+e.inspect()+" results refer-free "+n);
      return return_free(n);
    }
    else{
      var sym = x.name;
      return return_global(sym);
    }
    //throw new BiwaScheme.Error("undefined symbol `" + sym + "'");
  },

  //generate boxing code (intersection of sets & vars)
  //if no need of boxing, just returns next
  //  sets(Set): assigned variables 
  //  vars(List): used variables
  //  next(opc):
  //  ret(opc):
  make_boxes: function(sets, vars, next){
    var vars = vars;
    var n = 0;
    var a = [];
    while(vars instanceof BiwaScheme.Pair){
      if(sets.member(vars.car))
        a.push(n);
      n++;
      vars = vars.cdr;
    }
    var opc = next;
    for(var i=a.length-1; i>=0; i--)
      opc = ["box", a[i], opc];
    return opc;
  },

  // Enumerate variables which (could be assigned && included in v)
  // x: exp
  // v: set(vars)
  // ret: set
  find_sets: function(x, v){
    //Console.puts("find_sets: " + to_write(x) + " " + to_write(v))
    var ret=null;
    if(x instanceof BiwaScheme.Symbol){
      ret = new BiwaScheme.Set();
    }
    else if(x instanceof BiwaScheme.Pair){
      switch(x.first()){
      case BiwaScheme.Sym("define"):
        var exp=x.third();
        ret = this.find_sets(exp, v);
      case BiwaScheme.Sym("begin"):
        ret = this.find_sets(x.cdr, v); //(ignores improper list)
        break;
      case BiwaScheme.Sym("quote"):
        ret = new BiwaScheme.Set();
        break;
      case BiwaScheme.Sym("lambda"):
        var vars=x.second(), body=x.cdr.cdr;
        if (vars instanceof BiwaScheme.Pair){ // (lambda (...) ...)
          ret = this.find_sets(body, v.set_minus(vars.to_set()));
        }
        else { // (lambda args ...)
          ret = this.find_sets(body, v.set_minus(new BiwaScheme.Set(vars)));
        }
        break;
      case BiwaScheme.Sym("if"):
        var testc=x.second(), thenc=x.third(), elsec=x.fourth();
        ret = this.find_sets(testc, v).set_union(
                        this.find_sets(thenc, v),
                        this.find_sets(elsec, v));
        break;
      case BiwaScheme.Sym("set!"):
        var vari=x.second(), xx=x.third();
        if(v.member(vari))
          ret = this.find_sets(xx, v).set_cons(vari);
        else
          ret = this.find_sets(xx, v);
        break;
      case BiwaScheme.Sym("call/cc"):
        var exp=x.second();
        ret = this.find_sets(exp, v);
        break;
      default:
        var set = new BiwaScheme.Set();
        for(var p=x; p instanceof BiwaScheme.Pair; p=p.cdr){
          set = set.set_union(this.find_sets(p.car, v));
        }
        ret = set;
        break;
      }
    }
    else{
      ret = new BiwaScheme.Set();
    }

    if(ret == null)
      throw new BiwaScheme.Bug("find_sets() exited in unusual way");
    else
      return ret;
  },

  // find_free(): find free variables in x
  //              these variables are collected by collect_free().
  // x: expression 
  // b: set of local vars (= variables which are not free)
  // f: set of free var candidates 
  //    (local vars of outer lambdas)
  // ret: set of free vars
  find_free: function(x, b, f){
    var ret=null;
    if(x instanceof BiwaScheme.Symbol){
      if(f.member(x))
        ret = new BiwaScheme.Set(x);
      else
        ret = new BiwaScheme.Set();
    }
    else if(x instanceof BiwaScheme.Pair){
      switch(x.first()){
      case BiwaScheme.Sym("define"):
        var exp=x.third();
        ret = this.find_free(exp, b, f);
        break;
      case BiwaScheme.Sym("begin"):
        ret = this.find_free(x.cdr, b, f); //(ignores improper list)
        break;
      case BiwaScheme.Sym("quote"):
        ret = new BiwaScheme.Set();
        break;
      case BiwaScheme.Sym("lambda"):
        var vars=x.second(), body=x.cdr.cdr;
        if (vars instanceof BiwaScheme.Pair){ // (lambda (...) ...)
          ret = this.find_free(body, b.set_union(vars.to_set()), f);
        }
        else { // (lambda args ...)
          ret = this.find_free(body, b.set_cons(vars), f);
        }
        break;
      case BiwaScheme.Sym("if"):
        var testc=x.second(), thenc=x.third(), elsec=x.fourth();
        ret = this.find_free(testc, b, f).set_union(
                        this.find_free(thenc, b, f),
                        this.find_free(elsec, b, f));
        break;
      case BiwaScheme.Sym("set!"):
        var vari=x.second(), exp=x.third();
        if(f.member(vari))
          ret = this.find_free(exp, b, f).set_cons(vari);
        else
          ret = this.find_free(exp, b, f)
        break;
      case BiwaScheme.Sym("call/cc"):
        var exp=x.second();
        ret = this.find_free(exp, b, f);
        break;
      default:
        var set = new BiwaScheme.Set();
        for(var p=x; p instanceof BiwaScheme.Pair; p=p.cdr){
          set = set.set_union(this.find_free(p.car, b, f));
        }
        ret = set;
        break;
      }
    }
    else{
      ret = new BiwaScheme.Set();
    }
    //Console.p("find_free "+x.inspect()+" / "+b.inspect()+" => "+ret.inspect());

    if(ret == null)
      throw new BiwaScheme.Bug("find_free() exited in unusual way");
    else
      return ret;
  },

  // Returns the position of the dot pair.
  // Returns -1 if x is a proper list.
  //
  // eg. (a b . c) -> 2
  find_dot_pos: function(x){
    var idx = 0;
    for (; x instanceof BiwaScheme.Pair; x = x.cdr, ++idx)
      ;
    if (x != BiwaScheme.nil) {
      return idx;
    } else {
      return -1;
    }
  },

  last_pair: function(x){
    if (x instanceof BiwaScheme.Pair){
      for (; x.cdr instanceof BiwaScheme.Pair; x = x.cdr)
        ;
    }
    return x;
  },

  // Takes an dotted list and returns proper list.
  //
  // eg. (x y . z) -> (x y z)
  dotted2proper: function(ls){
    var nreverse = function(ls){
      var res = BiwaScheme.nil;
      for (; ls instanceof BiwaScheme.Pair; ){
        var d = ls.cdr;
        ls.cdr = res;
        res = ls;
        ls = d;
      }
      return res;
    }
    var copy_list = function(ls){
      var res = BiwaScheme.nil;
      for (; ls instanceof BiwaScheme.Pair; ls = ls.cdr){
        res = new BiwaScheme.Pair(ls.car, res);
      }
      return nreverse(res);
    }

    if (ls instanceof BiwaScheme.Pair) {
      var last = this.last_pair(ls);
      if (last instanceof BiwaScheme.Pair && last.cdr === BiwaScheme.nil){
        return ls;
      } else {
        var copied = copy_list(ls);
        this.last_pair(copied).cdr = new BiwaScheme.Pair(last.cdr, BiwaScheme.nil);
        return copied;
      }
    } else {
      return new BiwaScheme.Pair(ls, BiwaScheme.nil);
    }
  },

  // x: exp(list of symbol or integer or..)
  // e: env (= [locals, frees])
  // s: vars might be set!
  // next: opc
  // ret: opc
  compile: function(x, e, s, f, next){
    //Console.p(x);
    var ret = null;

    while(1){
      if(x instanceof BiwaScheme.Symbol){
        // Variable reference
        // compiled into refer-(local|free|global)
        return this.compile_refer(x, e, (s.member(x) ? ["indirect", next] : next));
      }
      else if(x instanceof BiwaScheme.Pair){
        switch(x.first()){
        case BiwaScheme.Sym("define"):
          ret = this._compile_define(x, next);

          x = ret[0];
          next = ret[1];
          break;

        case BiwaScheme.Sym("begin"):
          var a = [];
          for(var p=x.cdr; p instanceof BiwaScheme.Pair; p=p.cdr)
            a.push(p.car);

          //compile each expression (in reverse order)
          var c = next;
          for(var i=a.length-1; i>=0; i--){
            c = this.compile(a[i], e, s, f, c);
          }
          return c;

        case BiwaScheme.Sym("quote"):
          if(x.length() < 2)
              throw new BiwaScheme.Error("Invalid quote: "+x.to_write());

          var obj=x.second();
          return ["constant", obj, next];

        case BiwaScheme.Sym("lambda"):
          return this._compile_lambda(x, e, s, f, next);

        case BiwaScheme.Sym("if"):
          if(x.length() < 3 || x.length() > 4)
              throw new BiwaScheme.Error("Invalid if: "+x.to_write());

          var testc=x.second(), thenc=x.third(), elsec=x.fourth();
          var thenc = this.compile(thenc, e, s, f, next);
          var elsec = this.compile(elsec, e, s, f, next);
          x    = testc;
          next = ["test", thenc, elsec];
          break;

        case BiwaScheme.Sym("set!"):
          // error-checking: should have only 3 things
          if(x.length() != 3)
              throw new BiwaScheme.Error("Invalid set!: "+x.to_write());

          var v=x.second(), x=x.third();
          var do_assign = this.compile_lookup(v, e,
            function(n){ return ["assign-local", n, next]; },
            function(n){ return ["assign-free",  n, next]; },
            function(sym){ return ["assign-global",sym, next]; }
          );
          next = do_assign;
          break;

        case BiwaScheme.Sym("call/cc"): 
          var x=x.second();
          var c = ["conti", 
                    (this.is_tail(next) ? (e[0].size() + 1) : 0), //number of args for outer lambda
                    ["argument",
                    ["constant", 1,
                    ["argument",
                      this.compile(x, e, s,f,  
                        (this.is_tail(next) ? ["shift", 1, ["tco_hinted_apply"]]
                                            : ["apply"]))]]]];
                  //note: proc for call/cc takes 1 argument (= ["apply", 1])

          // Do not push stack frame when call/cc is in a tail context
          return this.is_tail(next) ? c : ["frame", c, next];

        default: 
          //apply 
          //x = (func 1 2) 
          //x.car = func = '(lambda (x) ..) or Symbol
          //x.cdr = args = '(1 2)
          var func = x.car;
          var args = x.cdr;
          var c = this.compile(func, e, s,f,  
                    this.is_tail(next) ? ["shift", args.length(), ["tco_hinted_apply"]]
                                       : ["apply"]);

          // VM will push the number of arguments to the stack.
          c = this.compile(args.length(), e, s, f, ["argument", c]);
          for(var p=args; p instanceof BiwaScheme.Pair; p=p.cdr){
            c = this.compile(p.car, e, s, f, ["argument", c]);
          }

          // Do not push stack frame for tail calls
          return this.is_tail(next) ? c : ["frame", c, next];
        }
      }
      else{
        return ["constant", x, next];
      }
    }
    //Console.p("result of " + x.inspect() + ":");
    //Console.p(ret);
    //dump({"ret":ret, "x":x, "e":e, "s":s, "next":next, "stack":[]});
//      if(ret == null)
//        throw new BiwaScheme.Bug("compile() exited in unusual way");
//      else
//        return ret;
  },

  // Compile define.
  //
  // 0. (define) ; => error
  // 1. (define a)
  // 2. (define a 1)
  // 3. (define a 1 2) ; => error
  // 4. (define (f x) x), (define (f . a) a)
  // 5. (define 1 2) 
  //
  // Note: define may appear in lambda, let, let*, let-values,
  // let*-values, letrec, letrec*. These definitions are local to the
  // <body> of these forms.
  _compile_define: function(x, next){
    if(x.length() == 1) { // 0. (define)
      throw new BiwaScheme.Error("Invalid `define': "+x.to_write());
    }

    var first = x.cdr.car;
    var rest = x.cdr.cdr;
    
    if(first instanceof BiwaScheme.Symbol){    
      if (rest === BiwaScheme.nil) { // 1. (define a)
        x = BiwaScheme.undef;
      }
      else {
        if (rest.cdr === BiwaScheme.nil) // 2. (define a 1)
          x = rest.car;
        else // 3. (define a 1 2)
          throw new BiwaScheme.Error("Invalid `define': "+x.to_write());
      }

      if (!BiwaScheme.TopEnv.hasOwnProperty(first.name)) {
        BiwaScheme.TopEnv[first.name] = BiwaScheme.undef;
      }
      next = ["assign-global", first.name, next];
    }
    else if(first instanceof BiwaScheme.Pair){ // 4. (define (f x) ...)
      // Note: define of this form may contain internal define.
      // They are handled in compilation of "lambda".

      var fname=first.car, args=first.cdr;
      var lambda = new BiwaScheme.Pair(BiwaScheme.Sym("lambda"), new BiwaScheme.Pair(args, rest));
      x = lambda;
      if (!BiwaScheme.TopEnv.hasOwnProperty(first.name)) {
        BiwaScheme.TopEnv[fname.name] = BiwaScheme.undef;
      }
      next = ["assign-global", fname.name, next];
    }
    else{ // 5. (define 1 2)
      throw new BiwaScheme.Error("define: symbol or pair expected but got "+first);
    }

    return [x, next];
  },

  // Compiles various forms of "lambda".
  //
  // * (lambda (x y) ...)
  // * (lambda (x y . rest) ...)
  // * (lambda args ...)
  _compile_lambda: function(x, e, s, f, next){
    if(x.length() < 3)
      throw new BiwaScheme.Error("Invalid lambda: "+x.to_write());

    var vars = x.cdr.car;
    var body = x.cdr.cdr;

    // Handle internal defines
    var tbody = BiwaScheme.Compiler.transform_internal_define(body);
    if(BiwaScheme.isPair(tbody) &&
       BiwaScheme.isSymbol(tbody.car) &&
       tbody.car.name == "letrec*"){
      // The body has internal defines.
      // Expand letrec* macro
      var cbody = BiwaScheme.Interpreter.expand(tbody);
    }
    else{
      // The body has no internal defines.
      // Just wrap the list with begin 
      var cbody = new BiwaScheme.Pair(BiwaScheme.Sym("begin"), x.cdr.cdr);
    }

    var dotpos = this.find_dot_pos(vars);
    var proper = this.dotted2proper(vars);
    var free = this.find_free(cbody, proper.to_set(), f); //free variables
    var sets = this.find_sets(cbody, proper.to_set());    //local variables

    var do_body = this.compile(cbody,
                    [proper.to_set(), free],
                    sets.set_union(s.set_intersect(free)),
                    f.set_union(proper.to_set()),
                    ["return"]);
    var do_close = ["close", 
                     free.size(),
                     this.make_boxes(sets, proper, do_body),
                     next,
                     dotpos];
    return this.collect_free(free, e, do_close);
  },

  run: function(expr){
    return this.compile(expr, [new BiwaScheme.Set(), new BiwaScheme.Set()], new BiwaScheme.Set(), new BiwaScheme.Set(), ["halt"]);
  }
});

// Compile an expression with new compiler
BiwaScheme.Compiler.compile = function(expr, next){
  expr = BiwaScheme.Interpreter.expand(expr);
  return (new BiwaScheme.Compiler).run(expr, next);
};

// Transform internal defines to letrec*.
//
// Example
//   (let ((a 1))
//     (define (b) a)
//     (b))
//
//   (let ((a 1))
//     (letrec* ((b (lambda () a)))
//       (b)))
//
// x - expression starts with (define ...)
// 
// Returns a letrec* expression, or
// just returns x, when x does not contain definitions.
(function(){
// Returns true if x is a definition
var is_definition = function(x){
  return BiwaScheme.isPair(x) &&
         BiwaScheme.Sym("define") === x.car;
  // TODO: support "begin", nested "begin", "let(rec)-syntax"
};

// Convert function definition to lambda binding
//   (define a ..)         -> (a ..)
//   (define (f) ..)       -> (f (lambda () ..))
//   (define (f x . y) ..) -> (f (lambda (x . y) ..))
//   (define (f . a) ..)   -> (f (lambda a ..))
var define_to_lambda_bind = function(def){
  var sig  = def.cdr.car;
  var body = def.cdr.cdr;

  if (BiwaScheme.isSymbol(sig)) {
    var variable = sig;

    return new BiwaScheme.Pair(variable, body);
  }
  else {
    var variable = sig.car;
    var value = new BiwaScheme.Pair(BiwaScheme.Sym("lambda"),
                  new BiwaScheme.Pair(sig.cdr, body));

    return BiwaScheme.List(variable, value);
  }
};

BiwaScheme.Compiler.transform_internal_define = function(x){
  // 1. Split x into definitions and expressions
  var defs = [], item = x;
  while (is_definition(item.car)){
    defs.push(item.car);
    item = item.cdr;
  }
  var exprs = item;

  // 2. Return x if there is no definitions
  if (defs.length == 0)
    return x;
  
  // 3. Return (letrec* <bindings> <expressions>)
  var bindings = BiwaScheme.List.apply(null, _.map(defs, define_to_lambda_bind));
  return new BiwaScheme.Pair(BiwaScheme.Sym("letrec*"),
           new BiwaScheme.Pair(bindings, exprs));
};
})();
