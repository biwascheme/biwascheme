
if( typeof(BiwaScheme)!='object' ) BiwaScheme={}; with(BiwaScheme) {
  define_libfunc("write-to-string", 1, 1, function(ar){
    return to_write(ar[0]);
  })
  define_libfunc("html-escape", 1, 1, function(ar){
    assert_string(ar[0]);
    return ar[0].escapeHTML();
  })

  //
  // json
  //
  // json->sexp
  // Array -> list
  // Object -> alist
  // (number, boolean, string, 
  //
  BiwaScheme.json2sexp = function(json){
    switch(true){
    case Object.isNumber(json) || 
         Object.isString(json) ||
         json === true || json === false:
      return json;
    case Object.isArray(json):
      return json.map(function(item){ 
               return json2sexp(item);
             }).to_list();
    case typeof(json) == "object":
      var ls = nil;
      for(key in json){
        ls = new Pair(new Pair(key, json2sexp(json[key])),
               ls);
      }
      return ls;
    default:
      throw new Error("json->sexp: detected invalid value for json: "+Object.inspect(json));
    }
    throw new Bug("must not happen");
  }
  define_libfunc("json->sexp", 1, 1, function(ar){
    return json2sexp(ar[0]);
  })

  //
  //from Gauche
  //

  define_libfunc("string-concat", 1, 1, function(ar){
    assert_pair(ar[0]);
    return ar[0].to_array().join("");
  })
  define_libfunc("string-split", 2, 2, function(ar){
    assert_string(ar[0]);
    assert_string(ar[1]);
    return ar[0].split(ar[1]).to_list();
  })
  define_libfunc("string-join", 1, 2, function(ar){
    assert_pair(ar[0]);
    var delim = ""
    if(ar[1]){
      assert_string(ar[1]);
      delim = ar[1];
    }
    return ar[0].to_array().join(delim);
  })

  define_libfunc("intersperse", 2, 2, function(ar){
    var item = ar[0], ls = ar[1];
    assert_pair(ls);

    var ret = [];
    ls.to_array().reverse().each(function(x){
      ret.push(x);
      ret.push(item);
    });
    ret.pop();
    return ret.to_list();
  });

  //(define-macro (foo x) body ...)
  //(define-macro foo lambda)
  define_syntax("define-macro", function(x){
    var head = x.cdr.car;
    if(head instanceof Pair){
      var name = head.car;
      var args = head.cdr;
      var body = x.cdr.cdr;
      var lambda = new Pair(Sym("lambda"),
                     new Pair(args,
                       body))
    }
    else{
      var name = head;
      var lambda = x.cdr.cdr.car;
    }

    //[close, n_frees, do_body, next]
    var opc = Compiler.compile(lambda);
    if(opc[1] != 0)
      throw new Bug("you cannot use free variables in macro expander (or define-macro must be on toplevel)")
    var cls = [opc[2]];

    TopEnv[name.name] = new Syntax(function(sexp){
      var args = sexp.to_array();
      args.shift();
      var intp = new Interpreter();
      var result = intp.invoke_closure(cls, args);
      return result;
    })
  })

  var macroexpand_1 = function(x){
    if(x instanceof Pair){
      if(x.car instanceof Symbol && TopEnv[x.car.name] instanceof Syntax){
        var transformer = TopEnv[x.car.name];
        x = transformer.transform(x);
      }
      else
        throw new Error("macroexpand-1: `" + to_write_ss(x) + "' is not a macro");
    }
    return x;
  }
  define_syntax("%macroexpand", function(x){
    var expanded = (new Interpreter).expand(x.cdr.car);
    return [Sym("quote"), expanded].to_list();
  });
  define_syntax("%macroexpand-1", function(x){
    var expanded = macroexpand_1(x.cdr.car);
    return [Sym("quote"), expanded].to_list();
  });

  define_libfunc("macroexpand", 1, 1, function(ar){
    return (new Interpreter).expand(ar[0]);
  });
  define_libfunc("macroexpand-1", 1, 1, function(ar){
    return macroexpand_1(ar[0]);
  });

  define_libfunc("print", 1, null, function(ar){
    ar.map(function(item){
      puts(to_display(item), true);
    })
    puts(""); //newline
  })
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
  // srfi
  //
  
  // srfi-1 (list)
  define_libfunc("iota", 1, 3, function(ar){
    var count = ar[0];
    var start = ar[1] || 0;
    var step = (ar[2]==undefined) ? 1 : ar[2];
    assert_integer(count);
    assert_number(start);
    assert_number(step);

    var a = [], n = start;
    for(var i=0; i<count; i++){
      a.push(n);
      n += step;
    }
    return a.to_list();
  });

  // srfi-6 & Gauche (string port)
  BiwaScheme.Port.StringOutput = Class.create(Port, {
    initialize: function($super){
      this.buffer = [];
      $super(false, true);
    },
    put_string: function(str){
      this.buffer.push(str);
    },
    output_string: function(str){
      return this.buffer.join("");
    }
  });
  BiwaScheme.Port.StringInput = Class.create(Port, {
    initialize: function($super){
      $super(true, false);
    },
    get_string: function(after){
    }
  });
  define_libfunc("open-input-string", 1, 1, function(ar){
    assert_string(ar[0]);
    return new Port.StringInput(ar[0]);
  })
  define_libfunc("open-output-string", 0, 0, function(ar){
    return new Port.StringOutput();
  })
  define_libfunc("get-output-string", 1, 1, function(ar){
    assert_port(ar[0]);
    if(!(ar[0] instanceof Port.StringOutput))
      throw new Error("get-output-string: port must be made by 'open-output-string'");
    return ar[0].output_string();
  })

  // srfi-19 (time)
  //
//  // constants
//time-duration
//time-monotonic
//time-process
//time-tai
//time-thread
//time-utc
  // Current time and clock resolution
  define_libfunc("current-date", 0, 1, function(ar){
    //todo: tz-offset (ar[1])
    return new Date();
  })
//
//current-julian-day -> jdn
//current-modified-julian-day -> mjdn
//current-time [time-type] -> time
//time-resolution [time-type] -> integer
//  // Time object and accessors
//make-time type nanosecond second -> time
//time? object -> boolean
//time-type time -> time-type
//time-nanosecond time -> integer
//time-second time -> integer
//set-time-type! time time-type
//set-time-nanosecond! time integer
//set-time-second! time integer
//copy-time time1 -> time2 
//  // Time comparison procedures
//time<=? time1 time2 -> boolean
//time<? time1 time2 -> boolean
//time=? time1 time2 -> boolean
//time>=? time1 time2 -> boolean
//time>? time1 time2 -> boolean
//  // Time arithmetic procedures
//time-difference time1 time2 -> time-duration
//time-difference! time1 time2 -> time-duration
//add-duration time1 time-duration -> time
//add-duration! time1 time-duration -> time
//subtract-duration time1 time-duration -> time
//subtract-duration! time1 time-duration -> time
  // Date object and accessors
  // make-date
  define_libfunc("date?", 1, 1, function(ar){
    return (ar[0] instanceof Date);
  })
  define_libfunc("date-nanosecond", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getMilliseconds() * 1000000;
  })
  define_libfunc("date-millisecond", 1, 1, function(ar){ // not srfi-19
    assert_date(ar[0]);
    return ar[0].getMilliseconds();
  })
  define_libfunc("date-second", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getSeconds();
  })
  define_libfunc("date-minute", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getMinutes();
  })
  define_libfunc("date-hour", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getHours();
  })
  define_libfunc("date-day", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getDate();
  })
  define_libfunc("date-month", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getMonth() + 1; //Jan = 0 in javascript..
  })
  define_libfunc("date-year", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getFullYear();
  })
  //date-zone-offset
  //date-year-day
  define_libfunc("date-week-day", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getDay();
  })
  //date-week-number

  // Time/Date/Julian Day/Modified Julian Day Converters
  // (snipped)
  
  // Date to String/String to Date Converters
  // TODO: support locale
  //   * date_names
  //   * ~f 5.2 sec
  //   * ~p AM/PM
  //   * ~X 2007/01/01
  BiwaScheme.date_names = {
    weekday: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
    full_weekday: ["Sunday", "Monday", "Tuesday", 
      "Wednesday", "Thursday", "Friday", "Saturday"],
    month: ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
    full_month: ["January", "February", "March", "April",
      "May", "June", "July", "August", "September", 
      "Octorber", "November", "December"]
  }

  BiwaScheme.date2string = function(date, format){
    var zeropad  = function(n){ return n<10 ? "0"+n : ""+n }; 
    var spacepad = function(n){ return n<10 ? " "+n : ""+n }; 
    
    var getter = {
      a: function(x){ return date_names.weekday[x.getDay()] },
      A: function(x){ return date_names.full_weekday[x.getDay()] },
      b: function(x){ return date_names.month[x.getMonth()] },
      B: function(x){ return date_names.full_month[x.getMonth()] },
      c: function(x){ return x.toString() },
      d: function(x){ return zeropad(x.getDate()) },
      D: function(x){ return getter.d(x) + getter.m(x) + getter.y(x); },
      e: function(x){ return spacepad(x.getDate()) },
      f: function(x){ return x.getSeconds() + x.getMilliseconds()/1000; },
      h: function(x){ return date_names.month[x.getMonth()] },
      H: function(x){ return zeropad(x.getHours()) },
      I: function(x){ var h = x.getHours(); return zeropad(h<13 ? h : h-12) },
      j: function(x){ throw new Bug("not implemented: day of year") },
      k: function(x){ return spacepad(x.getHours()) },
      l: function(x){ var h = x.getHours(); return spacepad(h<13 ? h : h-12) },
      m: function(x){ return zeropad(x.getMonth()) },
      M: function(x){ return zeropad(x.getMinutes()) },
      n: function(x){ return "\n" },
      N: function(x){ throw new Bug("not implemented: nanoseconds") },
      p: function(x){ return x.getHours()<13 ? "AM" : "PM" },
      r: function(x){ return getter.I(x) + ":" + getter.M(x) + ":" + getter.S(x) + " " + getter.p(x) },
      s: function(x){ return Math.floor(x.getTime() / 1000) },
      S: function(x){ return zeropad(x.getSeconds()) },
      t: function(x){ return "\t" },
      T: function(x){ return getter.H(x) + ":" + getter.M(x) + ":" + getter.S(x) },
      U: function(x){ throw new Bug("not implemented: weeknum(0~, Sun)") },
      V: function(x){ throw new Bug("not implemented: weeknum(1~, Sun?)") },
      w: function(x){ return x.getDay() },
      W: function(x){ throw new Bug("not implemented: weeknum(0~, Mon)") },
      x: function(x){ throw new Bug("not implemented: weeknum(1~, Mon)") },
      X: function(x){ return getter.Y(x) + "/" + getter.m(x) + "/" + getter.d(x) },
      y: function(x){ return x.getFullYear() % 100 },
      Y: function(x){ return x.getFullYear() },
      z: function(x){ throw new Bug("not implemented: time-zone") },
      Z: function(x){ throw new Bug("not implemented: symbol time zone") },
      1: function(x){ throw new Bug("not implemented: ISO-8601 year-month-day format") },
      2: function(x){ throw new Bug("not implemented: ISO-8601 hour-minute-second-timezone format") },
      3: function(x){ throw new Bug("not implemented: ISO-8601 hour-minute-second format") },
      4: function(x){ throw new Bug("not implemented: ISO-8601 year-month-day-hour-minute-second-timezone format") },
      5: function(x){ throw new Bug("not implemented: ISO-8601 year-month-day-hour-minute-second format") }
    }

    return format.replace(/~([\w1-5~])/g, function(_, x){
      var func = getter[x];
      if(func)
        return func(date);
      else if(x == "~")
        return "~";
      else
        return x;
    })
  }
  
  // date->string
  define_libfunc("date->string", 1, 2, function(ar){
    assert_date(ar[0]);

    if(ar[1]){
      assert_string(ar[1]);
      return date2string(ar[0], ar[1]);
    }
    else
      return ar[0].toString();
  })
  // string->date

  define_libfunc("parse-date", 1, 1, function(ar){ // not srfi-19
    assert_string(ar[0]);
    return new Date(Date.parse(ar[0]));
  })

  //
  // srfi-38 (write/ss)
  //
  var user_write_ss = function(ar){
    puts(write_ss(ar[0]), true);
  }
  define_libfunc("write/ss", 1, 2, user_write_ss);
  define_libfunc("write-with-shared-structure", 1, 2, user_write_ss);
  define_libfunc("write*", 1, 2, user_write_ss); //from Gauche(STklos)

  //
  // srfi-43 (vector library)
  //
  define_libfunc("vector-append", 2, null, function(ar){
    var vec = [];
    return vec.concat.apply(vec, ar);
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
    assert_regexp(ar[0], "regexp-exec");
    assert_string(ar[1], "regexp-exec");
    var ret = ar[0].exec(ar[1])
    return (ret === null) ? false : ret.to_list();
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
  //Function: regexp-replace-all regexp string substitution 
  //Function: regexp-replace* string rx1 sub1 rx2 sub2 ... 
  //Function: regexp-replace-all* string rx1 sub1 rx2 sub2 ... 
  //Function: regexp-quote string 
  //Macro: rxmatch-let match-expr (var ...) form ... 
  //Macro: rxmatch-if match-expr (var ...) then-form else-form 
  //Macro: rxmatch-cond clause ... 
  //Macro: rxmatch-case string-expr clause ... 

}


