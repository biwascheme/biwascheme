//
// R6RS Base library
//
function dump(){}

if( typeof(BiwaScheme)!='object' ) BiwaScheme={}; with(BiwaScheme) {
  /* --------------------------------------- namespace webscheme */ 

  ///
  /// Utility functions
  ///

  //
  // Iterator - external iterator
  //
  BiwaScheme.Iterator = {
    ForArray: Class.create({
      initialize: function(arr){
        this.arr = arr;
        this.i = 0;
      },
      has_next: function(){
        return this.i < this.arr.length;
      },
      next: function(){
        return this.arr[this.i++];
      }
    }),
    ForString: Class.create({
      initialize: function(str){
        this.str = str;
        this.i = 0;
      },
      has_next: function(){
        return this.i < this.str.length;
      },
      next: function(){
        return Char.get(this.str.charAt(this.i++));
      }
    }),
    ForList: Class.create({
      initialize: function(ls){
        this.ls = ls;
      },
      has_next: function(){
        return (this.ls instanceof Pair) && this.ls != nil;
      },
      next: function(){
        var pair = this.ls;
        this.ls = this.ls.cdr;
        return pair;
      }
    }),
    ForMulti: Class.create({
      initialize: function(objs){
        this.objs = objs;
        this.size = objs.length;
        this.iterators = objs.map(function(x){
          return Iterator.of(x);
        })
      },
      has_next: function(){
        for(var i=0; i<this.size; i++)
          if(!this.iterators[i].has_next())
            return false;
        
        return true;
      },
      next: function(){
        return this.iterators.map(function(ite){
          return ite.next();
        })
      }
    }),
    of: function(obj){
      switch(true){
        case (obj instanceof Array):
          return new this.ForArray(obj);
        case (typeof(obj) == "string"):
          return new this.ForString(obj);
        case (obj instanceof Pair):
          return new this.ForList(obj);
        default:
          throw new Bug("Iterator.of: unknown class: "+Object.inspect(obj));
      }
    }
  }

  //
  // Call.foreach - wrapper of Call
  //
  BiwaScheme.Call.default_callbacks = {
    call: function(x){ return new Call(this.proc, [x]) },
    result: Prototype.emptyFunction,
    finish: Prototype.emptyFunction
  }
  BiwaScheme.Call.foreach = function(obj, callbacks, is_multi){
    is_multi || (is_multi = false);
    ["call", "result", "finish"].each(function(key){
      if(!callbacks[key])
        callbacks[key] = Call.default_callbacks[key];
    })
    
    var iterator = null;
    var x = null;

    var loop = function(ar){
      if(iterator){
        var ret = callbacks["result"](ar[0], x);
        if(ret !== undefined) return ret;
        //Note: you can't return #<undef> from Call.foreach
      }
      else{
        if(is_multi)
          iterator = new Iterator.ForMulti(obj);
        else
          iterator = Iterator.of(obj);
      }

      if(!iterator.has_next())
        return callbacks["finish"]();
      else{
        x = iterator.next();
        var result = callbacks["call"](x);
        result.after = loop;
        return result;
      }
    }
    return loop(null);
  }
  BiwaScheme.Call.multi_foreach = function(obj, callbacks){
    return Call.foreach(obj, callbacks, true);
  }

  //
  // define_*func - define library functions
  //
  BiwaScheme.check_arity = function(len, min, max){
    var fname = arguments.callee.caller
                  ? arguments.callee.caller.fname 
                  : "";
    if(len < min){
      if(max && max == min)
        throw new Error(fname+": wrong number of arguments (expected: "+min+" got: "+len+")");
      else
        throw new Error(fname+": too few arguments (at least: "+min+" got: "+len+")");
    }
    else if(max && max < len)
      throw new Error(fname+": too many arguments (at most: "+max+" got: "+len+")");
  }
  BiwaScheme.define_libfunc = function(fname, min, max, func){
    var f = function(ar, intp){
      check_arity(ar.length, min, max);
      return func(ar, intp);
    };

    func["fname"] = fname; // for assert_*
    f["fname"]    = fname; // for check_arity
    f["inspect"] = function(){ return this.fname; }
    CoreEnv[fname] = f;
  }
  BiwaScheme.define_syntax = function(name, func){
    TopEnv[name] = new Syntax(func);
  }
  BiwaScheme.define_scmfunc = function(fname, min, max, str){
    (new Interpreter).evaluate("(define "+fname+" "+str+"\n)");
  }

  define_scmfunc("map+", 2, null, 
    "(lambda (proc ls) (if (null? ls) ls (cons (proc (car ls)) (map proc (cdr ls)))))");
  
  //
  // assert_* : type assertion
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
      if(!test(obj))
        throw new Error(fname + ": " + type + " required, but got " + to_write(obj));
    })
  }
  var assert_number = make_simple_assert("number", function(obj){
    return typeof(obj) == 'number' || (obj instanceof Complex);
  })
  var assert_integer = make_simple_assert("integer", function(obj){
    return typeof(obj) == 'number' && (obj % 1 == 0)
  })
  var assert_symbol = make_simple_assert("symbol", function(obj){
    return obj instanceof Symbol;
  })
  var assert_string = make_simple_assert("string", function(obj){
    return typeof(obj) == 'string';
  })
  var assert_vector = make_simple_assert("vector", function(obj){
    return obj instanceof Array;
  })
  var assert_pair = make_simple_assert("pair", function(obj){
    return obj instanceof Pair;
  })
  var assert_char = make_simple_assert("character", function(obj){
    return obj instanceof Char;
  })
  var assert_port = make_simple_assert("port", function(obj){
    return obj instanceof Port;
  })
  var assert_date = make_simple_assert("date", function(obj){
    return obj instanceof Date;
  })
  var assert_function = make_simple_assert("JavaScript function", function(obj){
    return (obj instanceof Function) || (typeof obj == 'function');
  })
  var assert_between = make_assert(function(fname, obj, from, to){
    if( typeof(obj) != 'number' || obj != Math.round(obj) )
      throw new Error(fname + ": " + "number required, but got " + to_write(obj));

    if( obj < from || to < obj )
      throw new Error(fname + ": " + "number must be between " + 
                      from + " and " + to + ", but got " + to_write(obj));
  })
  var assert = make_assert(function(fname, test){
  })

  ///
  /// Classes
  ///
  
  BiwaScheme.Complex = Class.create({
    initialize: function(real, imag){
      this.real = real;
      this.imag = imag;
    },
    magnitude: function(){
      return Math.sqrt(z.real * z.real + z.imag * z.imag);
    },
    angle: function(){
      return Math.acos(this.real / this.magnitude());
    }
  })
  BiwaScheme.Complex.from_polar = function(r, theta){
    var real = r * Math.cos(theta);
    var imag = r * Math.sin(theta);
    return new Complex(real, imag);
  }
  BiwaScheme.Complex.assure = function(num){
    if(num instanceof Complex)
      return num
    else
      return new Complex(num, 0);
  }

  BiwaScheme.Rational = Class.create({
    initialize: function(numerator, denominator){
      this.numerator = numerator;
      this.denominator = denominator;
    }
  })
  
  ///
  /// R6RS Base library
  ///
  
  //
  //        11.4  Expressions
  //
  //            11.4.1  Quotation
  //(quote)
  //            11.4.2  Procedures
  //(lambda)
  //            11.4.3  Conditionaar
  //(if)
  //            11.4.4  Assignments
  //(set!)
  //            11.4.5  Derived conditionaar

  define_syntax("cond", function(x){
    var clauses = x.cdr;
    if(!(clauses instanceof Pair) || clauses === nil){
      throw new Error("malformed cond: cond needs list but got " +
                      to_write_ss(clauses));
    }
    // TODO: assert that clauses is a proper list

    var ret = null;
    clauses.to_array().reverse().each(function(clause){
      if(!(clause instanceof Pair)){
        throw new Error("bad clause in cond: " + to_write_ss(clause));
      }

      if(clause.car === Sym("else")){ 
        if(ret !== null){
          throw new Error("'else' clause of cond followed by more clauses: " +
                          to_write_ss(clauses));
        }
        else if(clause.cdr === nil){
          // pattern A: (else)
          //  -> #f            ; not specified in R6RS...?
          ret = false;
        }
        else if(clause.cdr.cdr === nil){
          // pattern B: (else expr) 
          //  -> expr
          ret = clause.cdr.car;
        }
        else{
          // pattern C: (else expr ...)
          //  -> (begin expr ...)
          ret = new Pair(Sym("begin"), clause.cdr);
        }
      } 
      else if(ret === null){
        // pattern D: no else clause
        //  -> #<undef>
        ret = undefined;
      }
      else{
        var test = clause.car;
        if(clause.cdr === nil){
          // pattern 1: (test)
          //  -> (or test ret)
          ret = [Sym("or"), test, ret].to_list();
        }
        else if (clause.cdr.cdr === nil){
          // pattern 2: (test expr)
          //  -> (if test expr ret)
          ret = [Sym("if"), test, clause.cdr.car, ret].to_list();
        }
        else if(clause.cdr.car === Sym("=>")){
          // pattern 3: (test => expr)
          //  -> (let ((#<gensym1> test)) 
          //       (if test (expr #<gensym1>) ret))
          var test = clause.car, expr = clause.cdr.cdr.car;
          var tmp_sym = BiwaScheme.gensym();

          ret = List(Sym("let"), 
                     List( List(tmp_sym, test) ),
                     List(Sym("if"), test, List(expr, tmp_sym), ret));
        }
        else{
          // pattern 4: (test expr ...)
          //  -> (if test (begin expr ...) ret)
          ret = [Sym("if"), test, 
                   new Pair(Sym("begin"), clause.cdr),
                   ret].to_list();
        }
      }
    });
    return ret;
  });

  define_syntax("case", function(x){
    var tmp_sym = BiwaScheme.gensym();

    if(x.cdr === nil){
      throw new Error("case: at least one clause is required");
    }
    else if(!(x.cdr instanceof Pair)){
      throw new Error("case: proper list is required");
    }
    else{
      // (case key clauses ....)
      //  -> (let ((#<gensym1> key))
      var key = x.cdr.car;
      var clauses = x.cdr.cdr;

      var ret = undefined;
      clauses.to_array().reverse().each(function(clause){
        if(clause.car === Sym("else")){
          // pattern 0: (else expr ...)
          //  -> (begin expr ...)
          if(ret === undefined){
            ret = new Pair(Sym("begin"), clause.cdr);
          }
          else{
            throw new Error("case: 'else' clause followed by more clauses: " +
                            to_write_ss(clauses));
          }
        }
        else{
          // pattern 1: ((datum ...) expr ...)
          //  -> (if (or (eqv? key (quote d1)) ...) (begin expr ...) ret)
          ret = [
            Sym("if"),
            new Pair(Sym("or"), clause.car.to_array().map(function(d){
                return [Sym("eqv?"), 
                        tmp_sym, 
                        [Sym("quote"), d].to_list() ].to_list();
              }).to_list()),
            new Pair(Sym("begin"), clause.cdr),
            ret
          ].to_list();
        }
      });
      return new Pair(Sym("let1"),
               new Pair(tmp_sym,
                 new Pair(key,
                   new Pair(ret, nil))));
    }
  });

  define_syntax("and", function(x){
    // (and a b c) => (if a (if b c #f) #f)
    //todo: check improper list
    if(x.cdr == nil) return true;

    var objs = x.cdr.to_array();
    var i = objs.length-1;
    var t = objs[i];
    for(i=i-1; i>=0; i--)
      t = [Sym("if"), objs[i], t, false].to_list();

    return t;
  })

  define_syntax("or", function(x){
    // (or a b c) => (if a a (if b b (if c c #f)))
    //todo: check improper list

    var objs = x.cdr.to_array()
    var f = false;
    for(var i=objs.length-1; i>=0; i--)
      f = [Sym("if"), objs[i], objs[i], f].to_list();

    return f;
  })

  //            11.4.6  Binding constructs
  define_syntax("let", function(x){
    //(let ((a 1) (b 2)) (print a) (+ a b))
    //=> ((lambda (a b) (print a) (+ a b)) 1 2)
    //(let loop ((a 1) (b 2)) body .. (loop 3 4))
    //=> (letrec loop (lambda (loop a b) body .??.)
    //     (loop 1 2))
    //=> ((lambda (loop) (loop 1 2)) (lambda (a b) body ...))
    var binds = x.cdr.car, body = x.cdr.cdr;
    
    if(!(binds instanceof Pair)) 
      throw new Error("let: need a pair for bindings: got "+to_write(binds));

    var vars = nil, vals = nil;
    for(var p=binds; p instanceof Pair && p!=nil; p=p.cdr){
      vars = new Pair(p.car.car, vars);
      vals = new Pair(p.car.cdr.car, vals);
    }

    var lambda = new Pair(new Pair(Sym("lambda"), new Pair(vars, body)), vals);
    //puts(lambda.to_write());
    return lambda;
  })

  define_syntax("let*", function(x){
    //(let* ((a 1) (b a)) (print a) (+ a b))
    //-> (let ((a 1)) 
    //     (let ((b a)) (print a) (+ a b)))
    var binds = x.cdr.car, body = x.cdr.cdr;
    
    if(!(binds instanceof Pair)) 
      throw new Error("let*: need a pair for bindings: got "+to_write(binds));

    var ret = null;
    binds.to_array().reverse().each(function(bind){
      ret = new Pair(Sym("let"), 
               new Pair(new Pair(bind, nil),
                 ret == null ? body : new Pair(ret, nil)));
    })
    return ret;
  })

  var expand_letrec_star = function(x){
    var binds = x.cdr.car, body = x.cdr.cdr;
    
    if(!(binds instanceof Pair)) 
      throw new Error("letrec*: need a pair for bindings: got "+to_write(binds));

    var ret = body;
    binds.to_array().reverse().each(function(bind){
      ret = new Pair(new Pair(Sym("set!"), bind),
              ret);
    })
    var letbody = nil;
    binds.to_array().reverse().each(function(bind){
      letbody = new Pair(new Pair(bind.car, 
                           new Pair(undefined, nil)),
                  letbody);
    })
    return new Pair(Sym("let"),
             new Pair(letbody,
               ret));
  }
  define_syntax("letrec", expand_letrec_star);
  define_syntax("letrec*", expand_letrec_star);
  //let-values
  //let*-values
  //            11.4.7  Sequencing
  //(begin)

  //        
  //        11.5  Equivalence predicates
  //
  define_libfunc("eqv?", 2, 2, function(ar){
    return ar[0] == ar[1] && (typeof(ar[0]) == typeof(ar[1]));
  })
  define_libfunc("eq?", 2, 2, function(ar){
    return ar[0] === ar[1];
  })
  define_libfunc("equal?", 2, 2, function(ar){
    //TODO: must terminate for cyclic objects
    return to_write(ar[0]) == to_write(ar[1]);
  })

  //
  //        11.6  Procedure predicate
  //
  //"procedure?", 1, 1

  //
  //        11.7  Arithmetic
  //

  //            11.7.1  Propagation of exactness and inexactness
  //            11.7.2  Representability of infinities and NaNs
  //            11.7.3  Semantics of common operations
  //                11.7.3.1  Integer division
  //                11.7.3.2  Transcendental functions
  //(no functions are introduced by above sections)

  //
  //            11.7.4  Numerical operations
  //
  
  //                11.7.4.1  Numerical type predicates
  define_libfunc("number?", 1, 1, function(ar){
    return (typeof(ar[0]) == 'number') ||
           (ar[0] instanceof Complex)  ||
           (ar[0] instanceof Rational);
  });
  define_libfunc("complex?", 1, 1, function(ar){
    return (ar[0] instanceof Complex);
  });
  define_libfunc("real?", 1, 1, function(ar){
    return (typeof(ar[0]) == 'number');
  });
  define_libfunc("rational?", 1, 1, function(ar){
    return (ar[0] instanceof Rational);
  });
  define_libfunc("integer?", 1, 1, function(ar){
    return typeof(ar[0]) == 'number'  && 
           ar[0] == Math.round(ar[0]) &&
           ar[0] != Infinity          &&
           ar[0] != -Infinity;
  });

//(real-valued? obj)    procedure 
//(rational-valued? obj)    procedure 
//(integer-valued? obj)    procedure 
//
//(exact? z)    procedure 
//(inexact? z)    procedure

  //                11.7.4.2  Generic conversions
  //
//(inexact z)    procedure 
//(exact z)    procedure
//
  //                11.7.4.3  Arithmetic operations

  //inf & nan: ok (for this section)
  define_libfunc("=", 2, null, function(ar){
    var v = ar[0];
    assert_number(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_number(ar[i]);
      if(ar[i] != v) return false;
    }
    return true;
  });
  define_libfunc("<", 2, null, function(ar){
    assert_number(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_number(ar[i]);
      if(!(ar[i-1] < ar[i])) return false;
    }
    return true;
  });
  define_libfunc(">", 2, null, function(ar){
    assert_number(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_number(ar[i]);
      if(!(ar[i-1] > ar[i])) return false;
    }
    return true;
  });
  define_libfunc("<=", 2, null, function(ar){
    assert_number(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_number(ar[i]);
      if(!(ar[i-1] <= ar[i])) return false;
    }
    return true;
  });
  define_libfunc(">=", 2, null, function(ar){
    assert_number(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_number(ar[i]);
      if(!(ar[i-1] >= ar[i])) return false;
    }
    return true;
  });

  define_libfunc("zero?", 1, 1, function(ar){
    assert_number(ar[0]);
    return ar[0] === 0; 
  });
  define_libfunc("positive?", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] > 0);
  });
  define_libfunc("negative?", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] < 0);
  });
  define_libfunc("odd?", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] % 2 == 1) || (ar[0] % 2 == -1);
  })
  define_libfunc("even?", 1, 1, function(ar){
    assert_number(ar[0]);
    return ar[0] % 2 == 0;
  })
  define_libfunc("finite?", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] != Infinity) && (ar[0] != -Infinity) && !isNaN(ar[0]);
  })
  define_libfunc("infinite?", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] == Infinity) || (ar[0] == -Infinity);
  })
  define_libfunc("nan?", 1, 1, function(ar){
    assert_number(ar[0]);
    return isNaN(ar[0]);
  })
  define_libfunc("max", 2, null, function(ar){
    for(var i=0; i<ar.length; i++)
      assert_number(ar[i]);

    return Math.max.apply(null, ar)
  });
  define_libfunc("min", 2, null, function(ar){
    for(var i=0; i<ar.length; i++)
      assert_number(ar[i]);

    return Math.min.apply(null, ar);
  });

  define_libfunc("+", 0,null, function(ar){
    var n = 0;
    for(var i=0; i<ar.length; i++){
      assert_number(ar[i]);
      n+=ar[i];
    }
    return n;
  });
  define_libfunc("*", 0,null, function(ar){
    var n = 1;
    for(var i=0; i<ar.length; i++){
      assert_number(ar[i]);
      n*=ar[i];
    }
    return n;
  });
  define_libfunc("-", 1,null, function(ar){
    var len = ar.length;
    assert_number(ar[0]);

    if(len == 1)
      return -ar[0];
    else{
      var n = ar[0];
      for(var i=1; i<len; i++){
        assert_number(ar[i]);
        n-=ar[i];
      }
      return n;
    }
  });
  //for r6rs specification, (/ 0 0) or (/ 3 0) raises '&assertion exception'
  define_libfunc("/", 1,null, function(ar){
    var len = ar.length;
    assert_number(ar[0]);

    if(len == 1)
      return 1/ar[0];
    else{
      var n = ar[0];
      for(var i=1; i<len; i++){
        assert_number(ar[i]);
        n/=ar[i];
      }
      return n;
    }
  });

  define_libfunc("abs", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.abs(ar[0]);
  });

  var div = function(n, m){
    return Math.floor(n / m);
  }
  var mod = function(n, m){
    return n - Math.floor(n / m) * m;
  }
  var div0 = function(n, m){
    return (n > 0) ? Math.floor(n / m) : Math.ceil(n / m);
  }
  var mod0 = function(n, m){
    return (n > 0) ? n - Math.floor(n / m) * m 
                   : n - Math.ceil(n / m) * m;
  }
  define_libfunc("div0-and-mod0", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return new Values([div(ar[0], ar[1]), mod(ar[0], ar[1])]);
  })
  define_libfunc("div", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return div(ar[0], ar[1]);
  })
  define_libfunc("mod", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return mod(ar[0], ar[1]);
  })
  define_libfunc("div0-and-mod0", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return new Values([div0(ar[0], ar[1]), mod0(ar[0], ar[1])]);
  })
  define_libfunc("div0", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return div0(ar[0], ar[1]);
  })
  define_libfunc("mod0", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return mod0(ar[0], ar[1]);
  })

//(gcd n1 ...)    procedure 
//(lcm n1 ...)    procedure 

  define_libfunc("numerator", 1, 1, function(ar){
    assert_number(ar[0]);
    if(ar[0] instanceof Rational)
      return ar[0].numerator;
    else
      throw new Bug("todo");
  })
  define_libfunc("denominator", 1, 1, function(ar){
    assert_number(ar[0]);
    if(ar[0] instanceof Rational)
      return ar[0].denominator;
    else
      throw new Bug("todo");
  })
  define_libfunc("floor", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.floor(ar[0]);
  })
  define_libfunc("ceiling", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.ceil(ar[0]);
  })
  define_libfunc("truncate", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] < 0) ? Math.ceil(ar[0]) : Math.floor(ar[0]);
  })
  define_libfunc("round", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.round(ar[0]);
  })

//(rationalize x1 x2)    procedure 

  define_libfunc("exp", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.exp(ar[0]);
  })
  define_libfunc("log", 1, 2, function(ar){
    var num = ar[0], base = ar[1];
    assert_number(num);

    if(base){ // log b num == log e num / log e b
      assert_number(base);
      return Math.log(num) / Math.log(b)
    }
    else
      return Math.log(num);
  })
  define_libfunc("sin", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.sin(ar[0]);
  })
  define_libfunc("cos", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.cos(ar[0]);
  })
  define_libfunc("tan", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.tan(ar[0]);
  })
  define_libfunc("asin", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.asin(ar[0]);
  })
  define_libfunc("acos", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.asos(ar[0]);
  })
  define_libfunc("atan", 1, 2, function(ar){
    assert_number(ar[0]);
    if(ar[1]){
      assert_number(ar[1]);
      return Math.atan2(ar[0], ar[1]);
    }
    else
      return Math.atan(ar[0]);
  })
  define_libfunc("sqrt", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.sqrt(ar[0]);
  })
  define_libfunc("exact-integer-sqrt", 1, 1, function(ar){
    assert_number(ar[0]);
    var sqrt_f = Math.sqrt(ar[0]);
    var sqrt_i = sqrt_f - (sqrt_f % 1);
    var rest   = ar[0] - sqrt_i * sqrt_i;

    return new Values([sqrt_i, rest]);
  })
  define_libfunc("expt", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return Math.pow(ar[0], ar[1]);
  })
  define_libfunc("make-rectangular", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return new Complex(ar[0], ar[1]);
  })
  define_libfunc("make-polar", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return Complex.from_polar(ar[0], ar[1]);
  })
  define_libfunc("real-part", 1, 1, function(ar){
    assert_number(ar[0]);
    return Complex.assure(ar[0]).real;
  })
  define_libfunc("imag-part", 1, 1, function(ar){
    assert_number(ar[0]);
    return Complex.assure(ar[0]).imag;
  })
  define_libfunc("magnitude", 1, 1, function(ar){
    assert_number(ar[0]);
    return Complex.assure(ar[0]).magnitude();
  })
  define_libfunc("angle", 1, 1, function(ar){
    assert_number(ar[0]);
    return Complex.assure(ar[0]).angle();
  })

  //
  //                11.7.4.4  Numerical Input and Output
  //
  define_libfunc("number->string", 1, 3, function(ar){
    var z = ar[0], radix = ar[1], precision = ar[2];
    if(precision)
      throw new Bug("number->string: presition is not yet implemented");
    
    radix = radix || 10;  //TODO: check radix is 2, 8, 10, or 16.
    return z.toString(radix);
  })
  define_libfunc("string->number", 1, 3, function(ar){
    var s = ar[0], radix = ar[1] || 10;
    switch(s){
      case "+inf.0": return Infinity;
      case "-inf.0": return -Infinity;
      case "+nan.0": return NaN;
      default:       return parseInt(s, radix);
    }
  })

  //
  //        11.8  Booleans
  //

  define_libfunc("not", 1, 1, function(ar){
    return (ar[0] === false) ? true : false;
  });
  define_libfunc("boolean?", 1, 1, function(ar){
    return (ar[0] === false || ar[0] === true) ? true : false;
  });
  define_libfunc("boolean=?", 2, null, function(ar){
    var len = ar.length;
    for(var i=1; i<len; i++){
      if(ar[i] != ar[0]) return false;
    }
    return true;
  });

  //        11.9  Pairs and lists

  define_libfunc("pair?", 1, 1, function(ar){
    return (ar[0] instanceof Pair && ar[0] != nil) ? true : false;
  });
  define_libfunc("cons", 2, 2, function(ar){
    return new Pair(ar[0], ar[1]);
  });
  define_libfunc("car", 1, 1, function(ar){
    //should raise &assertion for '()...
    if(!ar[0] instanceof Pair) throw new Error("cannot take car of " + ar[0]);
    return ar[0].car;
  });
  define_libfunc("cdr", 1, 1, function(ar){
    //should raise &assertion for '()...
    if(!ar[0] instanceof Pair) throw new Error("cannot take cdr of " + ar[0]);
    return ar[0].cdr;
  });

  define_libfunc("caar", 1, 1, function(ar){ return ar[0].car.car; });
  define_libfunc("cadr", 1, 1, function(ar){ return ar[0].cdr.car; });
  define_libfunc("cdar", 1, 1, function(ar){ return ar[0].car.cdr; });
  define_libfunc("cddr", 1, 1, function(ar){ return ar[0].cdr.cdr; });
  define_libfunc("caaar", 1, 1, function(ar){ return ar[0].car.car.car; });
  define_libfunc("caadr", 1, 1, function(ar){ return ar[0].cdr.car.car; });
  define_libfunc("cadar", 1, 1, function(ar){ return ar[0].car.cdr.car; });
  define_libfunc("caddr", 1, 1, function(ar){ return ar[0].cdr.cdr.car; });
  define_libfunc("cdaar", 1, 1, function(ar){ return ar[0].car.car.cdr; });
  define_libfunc("cdadr", 1, 1, function(ar){ return ar[0].cdr.car.cdr; });
  define_libfunc("cddar", 1, 1, function(ar){ return ar[0].car.cdr.cdr; });
  define_libfunc("cdddr", 1, 1, function(ar){ return ar[0].cdr.cdr.cdr; });
  define_libfunc("caaaar", 1, 1, function(ar){ return ar[0].car.car.car.car; });
  define_libfunc("caaadr", 1, 1, function(ar){ return ar[0].cdr.car.car.car; });
  define_libfunc("caadar", 1, 1, function(ar){ return ar[0].car.cdr.car.car; });
  define_libfunc("caaddr", 1, 1, function(ar){ return ar[0].cdr.cdr.car.car; });
  define_libfunc("cadaar", 1, 1, function(ar){ return ar[0].car.car.cdr.car; });
  define_libfunc("cadadr", 1, 1, function(ar){ return ar[0].cdr.car.cdr.car; });
  define_libfunc("caddar", 1, 1, function(ar){ return ar[0].car.cdr.cdr.car; });
  define_libfunc("cadddr", 1, 1, function(ar){ return ar[0].cdr.cdr.cdr.car; });
  define_libfunc("cdaaar", 1, 1, function(ar){ return ar[0].car.car.car.cdr; });
  define_libfunc("cdaadr", 1, 1, function(ar){ return ar[0].cdr.car.car.cdr; });
  define_libfunc("cdadar", 1, 1, function(ar){ return ar[0].car.cdr.car.cdr; });
  define_libfunc("cdaddr", 1, 1, function(ar){ return ar[0].cdr.cdr.car.cdr; });
  define_libfunc("cddaar", 1, 1, function(ar){ return ar[0].car.car.cdr.cdr; });
  define_libfunc("cddadr", 1, 1, function(ar){ return ar[0].cdr.car.cdr.cdr; });
  define_libfunc("cdddar", 1, 1, function(ar){ return ar[0].car.cdr.cdr.cdr; });
  define_libfunc("cddddr", 1, 1, function(ar){ return ar[0].cdr.cdr.cdr.cdr; });

  define_libfunc("null?", 1, 1, function(ar){
    return (ar[0] === nil);
  });
  define_libfunc("list?", 1, 1, function(ar){
    var contents = [];
    for(var o=ar[0]; o != nil; o=o.cdr){
      if(!(o instanceof Pair)) return false;
      if(contents.find(function(item){ return item === o.car}))
        return false; //cyclic
      contents.push(o.car);
    }
    return true;
  });
  define_libfunc("list", 0, null, function(ar){
    var l = nil;
    for(var i=ar.length-1; i>=0; i--)
      l = new Pair(ar[i], l);
    return l;
  });
  define_libfunc("length", 1, 1, function(ar){
    assert_pair(ar[0]);

    var n = 0;
    for(var o=ar[0]; o!=nil; o=o.cdr)
      n++;
    return n;
  });
  define_libfunc("append", 2, null, function(ar){
    var k = ar.length
    var ret = ar[--k];
    while(k--){
      ar[k].to_array().reverse().each(function(item){
        ret = new Pair(item, ret);
      });
    }
    return ret;
  });
  define_libfunc("reverse", 1, 1, function(ar){
    if(!ar[0] instanceof Pair) throw new Error("reverse needs pair but got " + ar[0]);

    var l = nil;
    for(var o=ar[0]; o!=nil; o=o.cdr)
      l = new Pair(o.car, l);
    return l;
  });
  define_libfunc("list-tail", 2, 2, function(ar){
    if(!ar[0] instanceof Pair) throw new Error("list-tail needs pair but got " + ar[0]);

    var o = ar[0];
    for(var i=0; i<ar[1]; i++){
      if(!o instanceof Pair) throw new Error("list-tail: the list is shorter than " + ar[1]);
      o = o.cdr;
    }
    return o;
  });
  define_libfunc("list-ref", 2, 2, function(ar){
    if(!ar[0] instanceof Pair) throw new Error("list-ref needs pair but got " + ar[0]);

    var o = ar[0];
    for(var i=0; i<ar[1]; i++){
      if(!o instanceof Pair) throw new Error("list-ref: the list is shorter than " + ar[1]);
      o = o.cdr;
    }
    return o.car;
  });
  define_libfunc("map", 2, null, function(ar){
    var proc = ar.shift(), lists = ar;
    lists.each(function(ls){ assert_pair(ls) });

    var a = [];
    return Call.multi_foreach(lists, {
      call: function(xs){ 
        return new Call(proc, xs.map(function(x){ return x.car }));
      },
      result: function(res){ a.push(res); },
      finish: function(){ return a.to_list(); }
    })
  })
  define_libfunc("for-each", 2, null, function(ar){
    var proc = ar.shift(), lists = ar;
    lists.each(function(ls){ assert_pair(ls) });

    return Call.multi_foreach(lists, {
      call: function(xs){ 
        return new Call(proc, xs.map(function(x){ return x.car }));
      }
    })
  })

  //        11.10  Symbols

  define_libfunc("symbol?", 1, 1, function(ar){
    return (ar[0] instanceof Symbol) ? true : false;
  });
  define_libfunc("symbol->string", 1, 1, function(ar){
    assert_symbol(ar[0]);
    return ar[0].name;
  });
  define_libfunc("symbol=?", 2, null, function(ar){
    assert_symbol(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_symbol(ar[i]);
      if(ar[i] != ar[0]) return false;
    }
    return true;
  });
  define_libfunc("string->symbol", 1, 1, function(ar){
    assert_string(ar[0]);
    return Sym(ar[0]);
  });

  //
  //        11.11  Characters
  //
  define_libfunc('char?', 1, 1, function(ar){
    return (ar[0] instanceof Char);
  });
  define_libfunc('char->integer', 1, 1, function(ar){
    assert_char(ar[0]);
    return ar[0].value.charCodeAt(0);
  })
  define_libfunc('integer->char', 1, 1, function(ar){
    assert_integer(ar[0]);
    return Char.get(String.fromCharCode(ar[0]));
  })

  var make_char_compare_func = function(test){
    return function(ar){
      assert_char(ar[0]);
      for(var i=1; i<ar.length; i++){
        assert_char(ar[i]);
        if(!test(ar[i-1].value, ar[i].value))
          return false;
      }
      return true;
    }
  }
  define_libfunc('char=?', 2, null, 
    make_char_compare_func(function(a, b){ return a == b }))
  define_libfunc('char<?', 2, null, 
    make_char_compare_func(function(a, b){ return a < b }))
  define_libfunc('char>?', 2, null, 
    make_char_compare_func(function(a, b){ return a > b }))
  define_libfunc('char<=?', 2, null, 
    make_char_compare_func(function(a, b){ return a <= b }))
  define_libfunc('char>=?', 2, null, 
    make_char_compare_func(function(a, b){ return a >= b }))

  //
  //        11.12  Strings
  //
  define_libfunc("string?", 1, 1, function(ar){
    return (typeof(ar[0]) == "string"); 
  })
  define_libfunc("make-string", 1, 2, function(ar){
    assert_integer(ar[0]);
    var c = " ";
    if(ar[1]){
      assert_char(ar[1]);
      c = ar[1].value;
    }
    return c.times(ar[0]);
  })
  define_libfunc("string", 1, null, function(ar){
    for(var i=0; i<ar.length; i++)
      assert_char(ar[i]);
    return ar.map(function(c){ return c.value }).join("");
  })
  define_libfunc("string-length", 1, 1, function(ar){
    assert_string(ar[0]);
    return ar[0].length;
  })
  define_libfunc("string-ref", 2, 2, function(ar){
    assert_string(ar[0]);
    assert_between(ar[1], 0, ar[0].length-1);
    return Char.get(ar[0].charAt([ar[1]]));
  })
  define_libfunc("string=?", 2, null, function(ar){
    assert_string(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_string(ar[i]);
      if(ar[0] != ar[i]) return false;
    }
    return true;
  })
  define_libfunc("string<?", 2, null, function(ar){
    assert_string(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_string(ar[i]);
      if(!(ar[i-1] < ar[i])) return false;
    }
    return true;
  })
  define_libfunc("string>?", 2, null, function(ar){
    assert_string(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_string(ar[i]);
      if(!(ar[i-1] > ar[i])) return false;
    }
    return true;
  })
  define_libfunc("string<=?", 2, null, function(ar){
    assert_string(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_string(ar[i]);
      if(!(ar[i-1] <= ar[i])) return false;
    }
    return true;
  })
  define_libfunc("string>=?", 2, null, function(ar){
    assert_string(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_string(ar[i]);
      if(!(ar[i-1] >= ar[i])) return false;
    }
    return true;
  })

  define_libfunc("substring", 3, 3, function(ar){
    assert_string(ar[0]);
    assert_integer(ar[1]);
    assert_integer(ar[2]);

    if(ar[1] < 0) throw new Error("substring: start too small: "+ar[1]);
    if(ar[2] < 0) throw new Error("substring: end too small: "+ar[2]);
    if(ar[0].length+1 <= ar[1]) throw new Error("substring: start too big: "+ar[1]);
    if(ar[0].length+1 <= ar[2]) throw new Error("substring: end too big: "+ar[2]);
    if(!(ar[1] <= ar[2])) throw new Error("substring: not start <= end: "+ar[1]+", "+ar[2]);
    
    return ar[0].substring(ar[1], ar[2]);
  })

  define_libfunc("string-append", 1, null, function(ar){
    for(var i=0; i<ar.length; i++)
      assert_string(ar[i]);
    
    return ar.join("");
  })
  define_libfunc("string->list", 1, 1, function(ar){
    assert_string(ar[0]);
    var chars = [];
    ar[0].scan(/./, function(s){ chars.push(Char.get(s)) });
    return chars.to_list();
  })
  define_libfunc("list->string", 1, 1, function(ar){
    assert_pair(ar[0]);
    return ar[0].to_array().map(function(c){ return c.value; }).join("");
  })
  define_libfunc("string-for-each", 2, null, function(ar){
    var proc = ar.shift(), strs = ar;
    strs.each(function(str){ assert_string(str) });
    
    return Call.multi_foreach(strs, {
      call: function(chars){ return new Call(proc, chars); }
    })
  })
  define_libfunc("string-copy", 1, 1, function(ar){
    // note: this is useless, because javascript strings are immutable
    assert_string(ar[0]);
    return ar[0];
  })


  //
  //        11.13  Vectors
  //
  define_libfunc("vector?", 1, 1, function(ar){
    return (ar[0] instanceof Array);
  })
  define_libfunc("make-vector", 1, 2, function(ar){
    assert_integer(ar[0]);
    var vec = new Array(ar[0]);

    if(ar.length == 2){
      for(var i=0; i<ar[0]; i++)
        vec[i] = ar[1];
    }
    return vec;
  })
  define_libfunc("vector", 1, null, function(ar){
    return ar;
  })
  define_libfunc("vector-length", 1, 1, function(ar){
    assert_vector(ar[0]);
    return ar[0].length;
  })
  define_libfunc("vector-ref", 2, 2, function(ar){
    assert_vector(ar[0]);
    assert_integer(ar[1]);

    return ar[0][ar[1]];
  })
  define_libfunc("vector-set!", 3, 3, function(ar){
    assert_vector(ar[0]);
    assert_integer(ar[1]);

    ar[0][ar[1]] = ar[2];
  })
  define_libfunc("vector->list", 1, 1, function(ar){
    assert_vector(ar[0]);
    return ar[0].to_list();
  })
  define_libfunc("list->vector", 1, 1, function(ar){
    assert_pair(ar[0]);
    return ar[0].to_array();
  })
  define_libfunc("vector-fill!", 2, 2, function(ar){
    assert_vector(ar[0]);
    var vec = ar[0], obj = ar[1];

    for(var i=0; i<vec.length; i++)
      vec[i] = obj;
    return vec;
  })
  define_libfunc("vector-map", 2, null, function(ar){
    var proc = ar.shift(), vecs = ar;
    vecs.each(function(vec){ assert_vector(vec) });

    var a = [];
    return Call.multi_foreach(vecs, {
      call: function(objs){ return new Call(proc, objs); },
      result: function(res){ a.push(res); },
      finish: function(){ return a; }
    })
  })
  define_libfunc("vector-for-each", 2, null, function(ar){
    var proc = ar.shift(), vecs = ar;
    vecs.each(function(vec){ assert_vector(vec) });

    return Call.multi_foreach(vecs, {
      call: function(objs){ return new Call(proc, objs); }
    })
  })

  //
  //        11.14  Errors and violations
  //
//(error who message irritant1 ...)    procedure 
//(assertion-violation who message irritant1 ...)    procedure 
//(assert <expression>)    syntax 
  
  //
  //        11.15  Control features
  //
  define_libfunc("apply", 2, null, function(ar){
    var proc = ar.shift(), rest_args = ar.pop(), args = ar;
    args = args.concat(rest_args.to_array());

    return new Call(proc, args);
  })
  define_syntax("call-with-current-continuation", function(x){
    return new Pair(Sym("call/cc"),
             x.cdr);
  })
  define_libfunc("values", 0, null, function(ar){
    return new Values(ar);
  })
  define_libfunc("call-with-values", 2, 2, function(ar){
    var producer = ar[0], consumer = ar[1];
    return new Call(producer, [], function(ar){
      var values = ar[0];
      if(!(values instanceof Values))
        throw new Error("values expected, but got "+to_write(values));

      return new Call(consumer, values.content);
    })
  })

  //
  //dynamic-wind
  
  //        11.16  Iteration
  //named let
  
  //        11.17  Quasiquotation
  //quasiquote
  var expand_qq = function(f, lv){
    if(f instanceof Symbol || f === nil){
      return [Sym("quote"), f].to_list();
    }
    else if(f instanceof Pair){
      var car = f.car;
      if(car instanceof Pair && car.car === Sym("unquote-splicing")){
        var lv = lv-1;
        if(lv == 0)
          return [ Sym("append"), 
                   f.car.cdr.car, 
                   expand_qq(f.cdr, lv+1) 
                 ].to_list();
        else
          return [ Sym("cons"), 
                   [Sym("list"), Sym("unquote-splicing"), expand_qq(f.car.cdr.car, lv)].to_list(),
                   expand_qq(f.cdr, lv+1)
                 ].to_list();
      }
      else if(car === Sym("unquote")){
        var lv = lv-1;
        if(lv == 0)
          return f.cdr.car;
        else
          return [ Sym("list"), 
                   [Sym("quote"), Sym("unquote")].to_list(),
                   expand_qq(f.cdr.car, lv)
                 ].to_list();
      }
      else if(car === Sym("quasiquote"))
        return [
                 Sym("list"), 
                 Sym("quasiquote"), 
                 expand_qq(f.cdr.car, lv+1)
               ].to_list();
      else
        return [
                 Sym("cons"), 
                 expand_qq(f.car, lv),
                 expand_qq(f.cdr, lv)
               ].to_list();
    }
    else if(f instanceof Array){
      throw new Bug("vector quasiquotation is not implemented yet");
    }
//      // `#(1 2 (unquote f))
//      // (vector 1 2 f)
//      // `#(1 2 (unquote-splicing f) 3)
//      // (vector-append
//      //   (vector 1 2)
//      //   f
//      //   (vector 3))
//      // `#(1 2 `#(3 ,,f) 4)
//      // (vector 1 2 `#(3 ,g) 4)
//      var len = f.length;
//      if(len == 0) return f;
//
//      var vecs = [[]];
//      for(var i=0; i<len; i++){
//        if(f[i] instanceof Pair){
//          if(f[i].car === Sym("unquote")){
//            var lv = lv - 1;
//            if(lv == 0)
//              vecs.last().push(f[i]);
//            else
//              vecs.push()
//          }
//      }
//
//      var car = f[0];
//      if(car === Sym("unquote")){
//        var lv = lv - 1;
//        if(lv == 0)
//          return f.cdr.car;
//        else
//          return [ Sym("vector"),
//                   [Sym("quote"), Sym("unquote")].to_list(),
//                   expand_qq(f.cdr.car, lv)
//                 ].to_list();
//      }
//      else{
////        return [ Sym("vector"),
////                 expand_qq(
//      }
//    }
//  }
    else
      return f;
  }
  define_syntax("quasiquote", function(x){
    return expand_qq(x.cdr.car, 1);
  })
  //unquote
  define_syntax("unquote", function(x){
    throw new Error("unquote(,) must be inside quasiquote(`)");
  })
  //unquote-splicing
  define_syntax("unquote-splicing", function(x){
    throw new Error("unquote-splicing(,@) must be inside quasiquote(`)");
  })
  
  //        11.18  Binding constructs for syntactic keywords
  //let-syntax
  //letrec-syntax
  
  //        11.19  Macro transformers
  //syntax-rules
  //identifier-syntax
  //
  
  //        11.20  Tail calls and tail contexts
  //(no library function introduced)


  ///
  /// R6RS Standard Libraries
  ///

  //
  // Chapter 1 Unicode
  //
//(char-upcase char)    procedure 
//(char-downcase char)    procedure 
//(char-titlecase char)    procedure 
//(char-foldcase char)    procedure 
//
//(char-ci=? char1 char2 char3 ...)    procedure 
//(char-ci<? char1 char2 char3 ...)    procedure 
//(char-ci>? char1 char2 char3 ...)    procedure 
//(char-ci<=? char1 char2 char3 ...)    procedure 
//(char-ci>=? char1 char2 char3 ...)    procedure 
//
//(char-alphabetic? char)    procedure 
//(char-numeric? char)    procedure 
//(char-whitespace? char)    procedure 
//(char-upper-case? char)    procedure 
//(char-lower-case? char)    procedure 
//(char-title-case? char)    procedure 
//
//(char-general-category char)    procedure 

//(string-upcase string)    procedure 
//(string-downcase string)    procedure 
//(string-titlecase string)    procedure 
//(string-foldcase string)    procedure
//
//(string-ci=? string1 string2 string3 ...)    procedure 
//(string-ci<? string1 string2 string3 ...)    procedure 
//(string-ci>? string1 string2 string3 ...)    procedure 
//(string-ci<=? string1 string2 string3 ...)    procedure 
//(string-ci>=? string1 string2 string3 ...)    procedure 
//
//(string-normalize-nfd string)    procedure 
//(string-normalize-nfkd string)    procedure 
//(string-normalize-nfc string)    procedure 
//(string-normalize-nfkc string)    procedure 

  //
  // Chapter 2 Bytevectors
  //

  //
  // Chapter 3 List utilities
  //
  define_libfunc("find", 2, 2, function(ar){
    var proc = ar[0], ls = ar[1];
    assert_pair(ls);
    return Call.foreach(ls, {
      call: function(x){ return new Call(proc, [x.car]) },
      result: function(res, x){ if(res) return x.car; },
      finish: function(){ return false }
    })
  })
  define_libfunc("for-all", 2, null, function(ar){
    var proc = ar.shift();
    var lists = ar;
    lists.each(function(ls){ assert_pair(ls) });

    var last = true; //holds last result which proc returns
    return Call.multi_foreach(lists, {
      call: function(pairs){ 
        return new Call(proc, pairs.map(function(x){ return x.car }));
      },
      result: function(res, pairs){ 
        if(res === false) return false;
        last = res;
      },
      finish: function(){ return last; }
    })
  })
  define_libfunc("exists", 2, null, function(ar){
    var proc = ar.shift();
    var lists = ar;
    lists.each(function(ls){ assert_pair(ls) });

    return Call.multi_foreach(lists, {
      call: function(pairs){ 
        return new Call(proc, pairs.map(function(x){ return x.car }));
      },
      result: function(res, pairs){ 
        if(res !== false) return res;
      },
      finish: function(){ return false; }
    })
  })
  define_libfunc("filter", 2, 2, function(ar){
    var proc = ar[0], ls = ar[1];
    assert_pair(ls);

    var a = [];
    return Call.foreach(ls, {
      call: function(x){ return new Call(proc, [x.car]) },
      result: function(res, x){ if(res) a.push(x.car); },
      finish: function(){ return a.to_list() }
    })
  })
  define_scmfunc("partition+", 2, 2, 
    "(lambda (proc ls)  \
       (define (partition2 proc ls t f) \
         (if (null? ls) \
           (values (reverse t) (reverse f)) \
           (if (proc (car ls)) \
             (partition2 proc (cdr ls) (cons (car ls) t) f) \
             (partition2 proc (cdr ls) t (cons (car ls) f))))) \
       (partition2 proc ls '() '()))");

  define_libfunc("partition", 2, 2, function(ar){
    var proc = ar[0], ls = ar[1];
    assert_pair(ls);

    var t = [], f = [];
    return Call.foreach(ls, {
      call: function(x){ return new Call(proc, [x.car]) },
      result: function(res, x){ 
        if(res) t.push(x.car); 
        else    f.push(x.car); 
      },
      finish: function(){ 
        return new Values([t.to_list(), f.to_list()]);
      }
    })
  })
  define_libfunc("fold-left", 3, null, function(ar){
    var proc = ar.shift(), accum = ar.shift(), lists = ar;
    lists.each(function(ls){ assert_pair(ls) });

    return Call.multi_foreach(lists, {
      call: function(pairs){ 
        var args = pairs.map(function(x){ return x.car });
        args.unshift(accum);
        return new Call(proc, args);
      },
      result: function(res, pairs){ accum = res; },
      finish: function(){ return accum; }
    })
  })
  define_libfunc("fold-right", 3, null, function(ar){
    var proc = ar.shift(), accum = ar.shift();
    var lists = ar.map(function(ls){
      // reverse each list
      assert_pair(ls);
      return ls.to_array().reverse().to_list();
    })

    return Call.multi_foreach(lists, {
      call: function(pairs){ 
        var args = pairs.map(function(x){ return x.car });
        args.push(accum);
        return new Call(proc, args);
      },
      result: function(res, pairs){ accum = res; },
      finish: function(){ return accum; }
    })
  })
  define_libfunc("remp", 2, 2, function(ar){
    var proc = ar[0], ls = ar[1];
    assert_pair(ls);

    var ret = [];
    return Call.foreach(ls, {
      call: function(x){ return new Call(proc, [x.car]) },
      result: function(res, x){ if(!res) ret.push(x.car); },
      finish: function(){ return ret.to_list(); }
    })
  })
  var make_remover = function(key){
    return function(ar){ 
      var obj = ar[0], ls = ar[1];
      assert_pair(ls);

      var ret = [];
      return Call.foreach(ls, {
        call: function(x){ 
          return new Call(TopEnv[key] || CoreEnv[key], [obj, x.car]) 
        },
        result: function(res, x){ if(!res) ret.push(x.car); },
        finish: function(){ return ret.to_list(); }
      })
    }
  }
  define_libfunc("remove", 2, 2, make_remover("equal?"));
  define_libfunc("remv", 2, 2, make_remover("eqv?"));
  define_libfunc("remq", 2, 2, make_remover("eq?"));

  define_libfunc("memp", 2, 2, function(ar){
    var proc = ar[0], ls = ar[1];
    assert_pair(ls);

    var ret = [];
    return Call.foreach(ls, {
      call: function(x){ return new Call(proc, [x.car]) },
      result: function(res, x){ if(res) return x; },
      finish: function(){ return false; }
    })
  })
  var make_finder = function(key){
    return function(ar){ 
      var obj = ar[0], ls = ar[1];
      assert_pair(ls);

      var ret = [];
      return Call.foreach(ls, {
        call: function(x){ 
          return new Call(TopEnv[key] || CoreEnv[key], [obj, x.car]) 
        },
        result: function(res, x){ if(res) return x; },
        finish: function(){ return false; }
      })
    }
  }
  define_libfunc("member", 2, 2, make_finder("equal?"));
  define_libfunc("memv", 2, 2, make_finder("eqv?"));
  define_libfunc("memq", 2, 2, make_finder("eq?"));
  
  define_libfunc("assp", 2, 2, function(ar){
    var proc = ar[0], als = ar[1];
    assert_pair(als);

    var ret = [];
    return Call.foreach(als, {
      call: function(x){ 
        if(x.car.car)
          return new Call(proc, [x.car.car]);
        else
          throw new Error("ass*: pair required but got "+to_write(x.car));
      },
      result: function(res, x){ if(res) return x.car; },
      finish: function(){ return false; }
    })
  })
  var make_assoc = function(key){
    return function(ar){ 
      var obj = ar[0], ls = ar[1];
      assert_pair(ls);

      var ret = [];
      return Call.foreach(ls, {
        call: function(x){ 
          if(x.car.car)
            return new Call(TopEnv[key] || CoreEnv[key], [obj, x.car.car]) 
          else
            throw new Error("ass*: pair required but got "+to_write(x.car));
        },
        result: function(res, x){ if(res) return x.car; },
        finish: function(){ return false; }
      })
    }
  }
  define_libfunc("assoc", 2, 2, make_assoc("equal?"));
  define_libfunc("assv", 2, 2, make_assoc("eqv?"));
  define_libfunc("assq", 2, 2, make_assoc("eq?"));

  define_libfunc("cons*", 1, null, function(ar){
    if(ar.length == 1)
      return ar[0];
    else{
      var ret = null;
      ar.reverse().each(function(x){
        if(ret){
          ret = new Pair(x, ret);
        }
        else
          ret = x;
      })
      return ret;
    }
  })

  //
  // Chapter 4 Sorting
  //
//(list-sort proc list)    procedure 
//(vector-sort proc vector)    procedure
//(vector-sort! proc vector)    procedure 

  //
  // Chapter 5 Control Structures
  //
  define_syntax("when", function(x){
    //(when test body ...) 
    //=> (if test (begin body ...) #<undef>)
    var test = x.cdr.car, body = x.cdr.cdr;

    return new Pair(Sym("if"), 
             new Pair(test,
               new Pair(new Pair(Sym("begin"), body),
                 new Pair(undefined, nil))));
  })
  define_syntax("unless", function(x){
    //(unless test body ...) 
    //=> (if (not test) (begin body ...) #<undef>)
    var test = x.cdr.car, body = x.cdr.cdr;

    return new Pair(Sym("if"), 
             new Pair(new Pair(Sym("not"), new Pair(test, nil)),
               new Pair(new Pair(Sym("begin"), body),
                 new Pair(undefined, nil))));
  })
//(do ((<variable1> <init1> <step1>)    syntax 
//(case-lambda <case-lambda clause> ...)    syntax

  //
  // Chapter 6 Records
  //
//eqv, eq
//(define-record-type <name spec> <record clause>*)    syntax 
//fields    auxiliary syntax 
//mutable    auxiliary syntax 
//immutable    auxiliary syntax 
//parent    auxiliary syntax 
//protocol    auxiliary syntax 
//sealed    auxiliary syntax 
//opaque    auxiliary syntax 
//nongenerative    auxiliary syntax 
//parent-rtd    auxiliary syntax
//
//(make-record-type-descriptor name    procedure
//(record-type-descriptor? obj)    procedure 
//(make-record-constructor-descriptor rtd    procedure 
//(record-constructor constructor-descriptor)    procedure
//(record-predicate rtd)    procedure
//(record-accessor rtd k)    procedure 
//(record-mutator rtd k)    procedure
//
//(record? obj)    procedure
//(record-rtd record)    procedure
//(record-type-name rtd)    procedure
//(record-type-parent rtd)    procedure
//(record-type-uid rtd)    procedure 
//(record-type-generative? rtd)    procedure 
//(record-type-sealed? rtd)    procedure
//(record-type-opaque? rtd)    procedure
//(record-type-field-names rtd)    procedure
//(record-field-mutable? rtd k)    procedure 

  //
  // Chapter 7 Exceptions and conditions
  //
//(with-exception-handler handler thunk)    procedure
//(guard (<variable>    syntax
//(raise obj)    procedure 
//(raise-continuable obj)    procedure
//
//&condition    condition type
//(condition condition1 ...)    procedure 
//(simple-conditions condition)    procedure
//(condition? obj)    procedure
//(condition-predicate rtd)    procedure 
//(condition-accessor rtd proc)    procedure
//
//&message    condition type 
//&warning    condition type 
//&serious    condition type 
//&error    condition type 
//&violation    condition type 
//&assertion    condition type 
//&irritants    condition type 
//&who    condition type 
//&non-continuable    condition type 
//&implementation-restriction    condition type 
//&lexical    condition type 
//&syntax    condition type 
//&undefined    condition type 

  //
  // Chapter 8 I/O
  //
//  //    8  I/O
//  //        8.1  Condition types
//&i/o    condition type 
//&i/o-read    condition type 
//&i/o-write    condition type 
//&i/o-invalid-position    condition type 
//&i/o-filename    condition type 
//&i/o-file-protection    condition type
//&i/o-file-is-read-only    condition type
//&i/o-file-already-exists    condition type 
//&i/o-file-does-not-exist    condition type
//&i/o-port    condition type 
//
//  //        8.2  Port I/O
//  //            8.2.1  File names
//  //(no function introduced)
//
//  //            8.2.2  File options
//(file-options <file-options symbol> ...)    syntax 
//
//  //            8.2.3  Buffer modes
//(buffer-mode <buffer-mode symbol>)    syntax  
//(buffer-mode? obj)    procedure
//
//  //            8.2.4  Transcoders
//(latin-1-codec)    procedure 
//(utf-8-codec)    procedure 
//(utf-16-codec)    procedure
//(eol-style <eol-style symbol>)    syntax
//(native-eol-style)    procedure
//&i/o-decoding    condition type
//&i/o-encoding    condition type 
//(error-handling-mode <error-handling-mode symbol>)    syntax 
//(make-transcoder codec)    procedure 
//(make-transcoder codec eol-style)    procedure 
//(make-transcoder codec eol-style handling-mode)    procedure
//(native-transcoder)    procedure
//(transcoder-codec transcoder)    procedure 
//(transcoder-eol-style transcoder)    procedure 
//(transcoder-error-handling-mode transcoder)    procedure 
//(bytevector->string bytevector transcoder)    procedure 
//(string->bytevector string transcoder)    procedure
//
  //            8.2.5  End-of-file object
  //8.3 (eof-object)    procedure 
  //8.3 (eof-object? obj)    procedure 

  //            8.2.6  Input and output ports
  define_libfunc("port?", 1, 1, function(ar){
    return (ar[0] instanceof Port);
  })
//(port-transcoder port)    procedure 
  define_libfunc("textual-port?", 1, 1, function(ar){
    assert_port(ar[0]);
    return !ar[0].is_binary;
  })
  define_libfunc("binary-port?", 1, 1, function(ar){
    assert_port(ar[0]);
    return ar[0].is_binary;
  })
//(transcoded-port binary-port transcoder)    procedure
//(port-has-port-position? port)    procedure 
//(port-position port)    procedure
//(port-has-set-port-position!? port)    procedure 
//(set-port-position! port pos)    procedure
  define_libfunc("close-port", 1, 1, function(ar){
    assert_port(ar[0]);
    ar[0].close();
  })
//(call-with-port port proc)    procedure

  //            8.2.7  Input ports
  //8.3 (input-port? obj)    procedure 
//(port-eof? input-port)    procedure 
//(open-file-input-port filename)    procedure
//(open-bytevector-input-port bytevector)    procedure
//(open-string-input-port string)    procedure 
//(standard-input-port)    procedure 
//8.3 (current-input-port)    procedure
//(make-custom-binary-input-port id read!    procedure
//(make-custom-textual-input-port id read!    procedure
//
//  //            8.2.8  Binary input
//(get-u8 binary-input-port)    procedure
//(lookahead-u8 binary-input-port)    procedure
//(get-bytevector-n binary-input-port count)    procedure 
//(get-bytevector-n! binary-input-port    procedure
//(get-bytevector-some binary-input-port)    procedure
//(get-bytevector-all binary-input-port)    procedure
//
//  //            8.2.9  Textual input
//(get-char textual-input-port)    procedure
//(lookahead-char textual-input-port)    procedure 
//(get-string-n textual-input-port count)    procedure
//(get-string-n! textual-input-port string start count)    procedure
//(get-string-all textual-input-port)    procedure 
//(get-line textual-input-port)    procedure
//(get-datum textual-input-port)    procedure
//
  //            8.2.10  Output ports
  //8.3 (output-port? obj)    procedure
//(flush-output-port output-port)    procedure 
//(output-port-buffer-mode output-port)    procedure 
//(open-file-output-port filename)    procedure 
//(open-bytevector-output-port)    procedure 
//(call-with-bytevector-output-port proc)    procedure 
//(open-string-output-port)    procedure   
//(call-with-string-output-port proc)    procedure 
//(standard-output-port)    procedure 
//(standard-error-port)    procedure 
//8.3 (current-output-port)    procedure 
//8.3 (current-error-port)    procedure 
//(make-custom-binary-output-port id    procedure
  //(make-custom-textual-output-port id write! get-position set-position! close)
//  define_libfunc("make-custom-textual-output-port", 5, 5, function(ar){
//    assert_string(ar[0]);
//    assert_closure(ar[1]);
//    assert_closure(ar[2]);
//    assert_closure(ar[3]);
//    assert_closure(ar[4]);
//    return new Port(ar[0], ar[1], ar[2], ar[3], ar[4]);
//  })
//
//  //            8.2.11  Binary output
//(put-u8 binary-output-port octet)    procedure
//(put-bytevector binary-output-port bytevector)    procedure 
//
  //            8.2.12  Textual output
  define_libfunc("put-char", 2, 2, function(ar){
    assert_port(ar[0]);
    assert_char(ar[1]);
    ar[0].put_string(ar[1].value);
  })
  define_libfunc("put-string", 2, 2, function(ar){
    assert_port(ar[0]);
    assert_string(ar[1]);
    ar[0].put_string(ar[1]);
  })
  define_libfunc("put-datum", 2, 2, function(ar){
    assert_port(ar[0]);
    ar[0].put_string(to_write(ar[1]));
  })
//
//  //            8.2.13  Input/output ports
//(open-file-input/output-port filename)    procedure 
//(make-custom-binary-input/output-port    procedure 
//(make-custom-textual-input/output-port    procedure
//
//  //        8.3  Simple I/O
  define_libfunc("eof-object", 0, 0, function(ar){
    return eof;
  })
  define_libfunc("eof-object?", 1, 1, function(ar){
    return ar[0] === eof;
  })
//(call-with-input-file filename proc)    procedure 
//(call-with-output-file filename proc)    procedure
  define_libfunc("input-port?", 1, 1, function(ar){
    assert_port(ar[0]);
    return ar[0].is_input;
  })
  define_libfunc("output-port?", 1, 1, function(ar){
    assert_port(ar[0]);
    return ar[0].is_output;
  })
  define_libfunc("current-input-port", 0, 0, function(ar){
    return Port.current_input;
  })
  define_libfunc("current-output-port", 0, 0, function(ar){
    return Port.current_output;
  })
  define_libfunc("current-error-port", 0, 0, function(ar){
    return Port.current_error;
  })
//(with-input-from-file filename thunk)    procedure 
//(with-output-to-file filename thunk)    procedure
//(open-input-file filename)    procedure
//(open-output-file filename)    procedure 
  define_libfunc("close-input-port", 1, 1, function(ar){
    assert_port(ar[0]);
    if(!ar[0].is_input)
      throw new Error("close-input-port: port is not input port");
    ar[0].close();
  });
  define_libfunc("close-output-port", 1, 1, function(ar){
    assert_port(ar[0]);
    if(!ar[0].is_output)
      throw new Error("close-output-port: port is not output port");
    ar[0].close();
  });
//(read-char)    procedure 
//(peek-char)    procedure 
  define_libfunc("read", 0, 0, function(ar){
    return Port.current_input.get_string(function(str){
      var parser = new Parser(str);
      return parser.getObject();
    })
  })

  // write-char [1,2]
  define_libfunc("newline", 0, 1, function(ar){
    var port = ar[0] || Port.current_output;
    port.put_string("\n");
  });
  define_libfunc("display", 1, 2, function(ar){
    var port = ar[1] || Port.current_output;
    port.put_string(to_display(ar[0]));
  });
  define_libfunc("write", 1, 2, function(ar){
    var port = ar[1] || Port.current_output;
    port.put_string(to_write(ar[0]));
  });

  //
  // Chapter 9 File System
  //
//(file-exists? filename)    procedure 
  define_libfunc("file-exists?", 1, 1, function(ar){
		netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect"); //TODO: extract to a function
    assert_string(ar[0]);
    var fileIn = FileIO.open(ar[0]);
    return fileIn.exists();
  });
//(delete-file filename)    procedure 
  define_libfunc("delete-file", 1, 1, function(ar){
		netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect"); //TODO: extract to a function
    assert_string(ar[0]);
    var deleted = FileIO.unlink(FileIO.open(ar[0]));
    if(!deleted){
      //TODO: raise %i/o-filename if not found or not deletable
      puts("delete-file: cannot delete " + ar[0]);
    }
    return undefined;
  });

  //
  // Chapter 10 Command-line access and exit values
  //
//(command-line)    procedure
//(exit)    procedure 
//(exit obj)    procedure

  //
  // Chapter 11 Arithmetic
  //
////        11.1  Bitwise operations
////        11.2  Fixnums
//(fixnum? obj)    procedure
//(fixnum-width)    procedure 
//(least-fixnum)    procedure 
//(greatest-fixnum)    procedure 
//(fx=? fx1 fx2 fx3 ...)    procedure 
//(fx>? fx1 fx2 fx3 ...)    procedure 
//(fx<? fx1 fx2 fx3 ...)    procedure 
//(fx>=? fx1 fx2 fx3 ...)    procedure 
//(fx<=? fx1 fx2 fx3 ...)    procedure 
//(fxzero? fx)    procedure 
//(fxpositive? fx)    procedure 
//(fxnegative? fx)    procedure 
//(fxodd? fx)    procedure 
//(fxeven? fx)    procedure
//(fxmax fx1 fx2 ...)    procedure 
//(fxmin fx1 fx2 ...)    procedure
//(fx+ fx1 fx2)    procedure 
//(fx* fx1 fx2)    procedure
//(fx- fx1 fx2)    procedure 
//(fxdiv-and-mod fx1 fx2)    procedure 
//(fxdiv fx1 fx2)    procedure 
//(fxmod fx1 fx2)    procedure 
//(fxdiv0-and-mod0 fx1 fx2)    procedure 
//(fxdiv0 fx1 fx2)    procedure 
//(fxmod0 fx1 fx2)    procedure 
//(fx+/carry fx1 fx2 fx3)    procedure
//(fx-/carry fx1 fx2 fx3)    procedure
//(fx*/carry fx1 fx2 fx3)    procedure 
//(fxnot fx)    procedure
//(fxand fx1 ...)    procedure 
//(fxior fx1 ...)    procedure 
//(fxxor fx1 ...)    procedure
//(fxif fx1 fx2 fx3)    procedure
//(fxbit-count fx)    procedure
//(fxlength fx)    procedure
//(fxfirst-bit-set fx)    procedure 
//(fxbit-set? fx1 fx2)    procedure
//(fxcopy-bit fx1 fx2 fx3)    procedure 
//(fxbit-field fx1 fx2 fx3)    procedure
//(fxcopy-bit-field fx1 fx2 fx3 fx4)    procedure
//(fxarithmetic-shift fx1 fx2)    procedure
//(fxarithmetic-shift-left fx1 fx2)    procedure 
//(fxarithmetic-shift-right fx1 fx2)    procedure
//(fxrotate-bit-field fx1 fx2 fx3 fx4)    procedure
//(fxreverse-bit-field fx1 fx2 fx3)    procedure
//
////        11.3  Flonums
//(flonum? obj)    procedure
//(real->flonum x)    procedure
//(fl=? fl1 fl2 fl3 ...)    procedure 
//(fl<? fl1 fl2 fl3 ...)    procedure 
//(fl<=? fl1 fl2 fl3 ...)    procedure 
//(fl>? fl1 fl2 fl3 ...)    procedure 
//(fl>=? fl1 fl2 fl3 ...)    procedure
//(flinteger? fl)    procedure 
//(flzero? fl)    procedure 
//(flpositive? fl)    procedure 
//(flnegative? fl)    procedure 
//(flodd? ifl)    procedure 
//(fleven? ifl)    procedure 
//(flfinite? fl)    procedure 
//(flinfinite? fl)    procedure 
//(flnan? fl)    procedure
//(flmax fl1 fl2 ...)    procedure 
//(flmin fl1 fl2 ...)    procedure
//(fl+ fl1 ...)    procedure 
//(fl* fl1 ...)    procedure 
//(fl- fl1 fl2 ...)    procedure 
//(fl- fl)    procedure 
//(fl/ fl1 fl2 ...)    procedure 
//(fl/ fl)    procedure 
//(flabs fl)    procedure
//(fldiv-and-mod fl1 fl2)    procedure 
//(fldiv fl1 fl2)    procedure 
//(flmod fl1 fl2)    procedure 
//(fldiv0-and-mod0 fl1 fl2)    procedure 
//(fldiv0 fl1 fl2)    procedure 
//(flmod0 fl1 fl2)    procedure 
//(flnumerator fl)    procedure 
//(fldenominator fl)    procedure 
//(flfloor fl)    procedure 
//(flceiling fl)    procedure 
//(fltruncate fl)    procedure 
//(flround fl)    procedure
//(flexp fl)    procedure 
//(fllog fl)    procedure 
//(fllog fl1 fl2)    procedure 
//(flsin fl)    procedure 
//(flcos fl)    procedure 
//(fltan fl)    procedure 
//(flasin fl)    procedure 
//(flacos fl)    procedure 
//(flatan fl)    procedure 
//(flatan fl1 fl2)    procedure
//(flsqrt fl)    procedure
//(flexpt fl1 fl2)    procedure 
//&no-infinities    condition type 
//&no-nans    condition type 
//(fixnum->flonum fx)    procedure 
//
////        11.4  Exact bitwise arithmetic
//(bitwise-not ei)    procedure
//(bitwise-and ei1 ...)    procedure 
//(bitwise-ior ei1 ...)    procedure 
//(bitwise-xor ei1 ...)    procedure 
//(bitwise-if ei1 ei2 ei3)    procedure
//(bitwise-bit-count ei)    procedure 
//(bitwise-length ei)    procedure
//(bitwise-first-bit-set ei)    procedure 
//(bitwise-bit-set? ei1 ei2)    procedure 
//(bitwise-copy-bit ei1 ei2 ei3)    procedure
//(bitwise-bit-field ei1 ei2 ei3)    procedure
//(bitwise-copy-bit-field ei1 ei2 ei3 ei4)    procedure 
//(bitwise-arithmetic-shift ei1 ei2)    procedure
//(bitwise-arithmetic-shift-left ei1 ei2)    procedure 
//(bitwise-arithmetic-shift-right ei1 ei2)    procedure 
//(bitwise-arithmetic-shift-right ei1 ei2)
//(bitwise-rotate-bit-field ei1 ei2 ei3 ei4)    procedure 
//(bitwise-reverse-bit-field ei1 ei2 ei3)    procedure 


  //
  // Chapter 12 syntax-case
  //

  //
  // Chapter 13 Hashtables
  //
//13.1  Constructors
//(make-eq-hashtable)    procedure 
//(make-eq-hashtable k)    procedure
//(make-eqv-hashtable)    procedure 
//(make-eqv-hashtable k)    procedure 
//(make-hashtable hash-function equiv)    procedure 
//(make-hashtable hash-function equiv k)    procedure
//13.2  Procedures
//(hashtable? hashtable)    procedure 
//(hashtable-size hashtable)    procedure 
//(hashtable-ref hashtable key default)    procedure 
//(hashtable-set! hashtable key obj)    procedure 
//(hashtable-delete! hashtable key)    procedure 
//(hashtable-contains? hashtable key)    procedure 
//(hashtable-update! hashtable key proc default)    procedure 
//(hashtable-copy hashtable)    procedure 
//(hashtable-copy hashtable mutable)    procedure 
//(hashtable-clear! hashtable)    procedure 
//(hashtable-clear! hashtable k)    procedure 
//(hashtable-keys hashtable)    procedure 
//(hashtable-entries hashtable)    procedure 
//13.3  Inspection
//(hashtable-equivalence-function hashtable)    procedure 
//(hashtable-hash-function hashtable)    procedure 
//(hashtable-mutable? hashtable)    procedure 
//13.4  Hash functions
//(equal-hash obj)    procedure 
//(string-hash string)    procedure
//(string-ci-hash string)    procedure
//(symbol-hash symbol)    procedure

  //
  // Chapter 14 Enumerators
  //
//(make-enumeration symbol-list)    procedure 
//(enum-set-universe enum-set)    procedure 
//(enum-set-indexer enum-set)    procedure
//(enum-set-constructor enum-set)    procedure 
//(enum-set->list enum-set)    procedure
//(enum-set-member? symbol enum-set)    procedure 
//(enum-set-subset? enum-set1 enum-set2)    procedure 
//(enum-set=? enum-set1 enum-set2)    procedure 
//(enum-set-union enum-set1 enum-set2)    procedure 
//(enum-set-intersection enum-set1 enum-set2)    procedure 
//(enum-set-difference enum-set1 enum-set2)    procedure 
//(enum-set-complement enum-set)    procedure 
//(enum-set-projection enum-set1 enum-set2)    procedure 
//(define-enumeration <type-name>    syntax 
//(<symbol> ...)
//<constructor-syntax>)

  //
  // Chapter 15 Composite library
  //
  //(rnrs 6) = all - eval - mutable pairs - mutable strings - r5rs compatibility

  //
  // Chapter 16 eval
  //
//(eval expression environment)    procedure 
  define_libfunc("eval", 1, 1, function(ar){
    //TODO: environment
    var src = ar[0]
    assert_string(src);
    return (new BiwaScheme.Interpreter).evaluate(src);
  });
//(environment import-spec ...)    procedure

  //
  // Chapter 17 Mutable pairs
  //
//(set-car! pair obj)    procedure
//(set-cdr! pair obj)    procedure

  //
  // Chapter 18 Mutable strings
  //
  //(string-set! string k char)    procedure
 // (string-fill! string char)    procedure 
 
  //
  // Chapter 19 R5RS compatibility
  //
//(exact->inexact z)    procedure 
//(inexact->exact z)    procedure 
//
//(quotient n1 n2)    procedure 
//(remainder n1 n2)    procedure 
//(modulo n1 n2)    procedure
//
//(delay <expression>)    syntax  
//(force promise)    procedure 
//(make-promise (lambda () <expression>))
//
//(null-environment n)    procedure 
//(scheme-report-environment n)    procedure 

  /* --------------------------------------- namespace webscheme */ 
}
