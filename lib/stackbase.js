// 
// Heap based scheme from 3imp.pdf
//

// default definition of puts: should be overriden for console interpreters
function puts(str, no_newline){
  var text = (str + (no_newline ? "" : "\n")).escapeHTML();
  var span = document.createElement("span");
  span.innerHTML = text.replace(/\n/g,"<br>").replace(/ /g,"&nbsp;");
  $('bs-console').insert(span);
}
function p(/*args*/){
  puts("p> "+$A(arguments).map(Object.inspect).join(" "));
}

if( typeof(BiwaScheme)!='object' ) BiwaScheme={}; with(BiwaScheme) {
  /* --------------------------------------- namespace webscheme */ 

  //
  // javascript extention
  //

//  Object.prototype.inspect = function() {
//    var a = [];
//    for(var k in this){
//      //if(this.prototype[k]) continue;
//      a.push( k.toString() );//+" => "+this[k].toString() );
//    }
//    return "#<Object{"+a.join(",")+"}>";
//  }
  Function.prototype.to_write = function() {
    return "#<Function "+(this.fname ? this.fname : this.toSource().truncate(40))+">";
  }
  String.prototype.to_write = function(){
    return '"' +
           this.replace(/\\|\"/g,function($0){return'\\'+$0;})
               .replace(/\x07/g, "\\a")
               .replace(/\x08/g, "\\b")
               .replace(/\t/g, "\\t")
               .replace(/\n/g, "\\n")
               .replace(/\v/g, "\\v")
               .replace(/\f/g, "\\f")
               .replace(/\r/g, "\\r") +
           '"';
  }
  //Number.prototype.inspect = function() { return this.toString(); }
  Array.prototype.to_write = function(){
    var a = [];
    for(var i=0; i<this.length; i++)
      a.push(to_write(this[i]));
    return '#(' + a.join(" ") + ')';
  }
  Array.prototype.to_list = function(){
    var list = nil;
    for(var i=this.length-1; i>=0; i--){
      list = new Pair(this[i], list);
    }
    return list;
  }
//  Array.prototype.memq = function(x){
//    for(var i=this.length-1; i>=0; i--){
//      if(this[i] === x)
//        return true;
//    }
//    return false;
//  }

  //
  // utility functions
  //
  BiwaScheme.to_write = function(obj){
    if(obj === undefined)
      return "undefined";
    else if(obj === null)
      return "null";
    else if(typeof(obj.to_write) == 'function')
      return obj.to_write();
    else if(isNaN(obj) && typeof(obj) == 'number')
      return "+nan.0";
    else{
      switch(obj){
        case true: return "#t";
        case false: return "#f";
        case nil: return "()";
        case Infinity: return "+inf.0";
        case -Infinity: return "-inf.0";
      }
    }
    return Object.inspect(obj);
  }
  BiwaScheme.to_display = function(obj){
    if(typeof(obj.valueOf()) == "string")
      return obj;
    else if(obj instanceof Symbol)
      return obj.name;
    else if(obj instanceof Array)
      return '#(' + obj.map(function(x){ return to_display(x) }).join(' ') + ')';
    else if(obj instanceof Pair)
      return obj.inspect(to_display);
    else if(obj instanceof Char)
      return obj.value;
    else
      return to_write(obj);
  }

  // write/ss (write with substructure)
  // example:  > (let ((x (list 'a))) (list x x))                   //           (#0=(a) #0#)
  // 2-pass algorithm.
  // (1) detect all the objects which appears more than once
  //     (find_cyclic, reduce_cyclic_info)
  // (2) write object using this information
  //   * add prefix '#n=' for first appearance
  //   * just write '#n#' for other appearance
  
  //TODO: support Values
  BiwaScheme.write_ss = function(obj, array_mode){
    var known = [obj], used = [false];
    find_cyclic(obj, known, used);
    var cyclic   = reduce_cyclic_info(known, used);
    var appeared = new Array(cyclic.length);
    for(var i=cyclic.length-1; i>=0; i--) appeared[i] = false;

    return to_write_ss(obj, cyclic, appeared, array_mode);
  }
  BiwaScheme.to_write_ss = function(obj, cyclic, appeared, array_mode){
    var ret = "";
    var i = cyclic.indexOf(obj);
    if(i >= 0){
      if(appeared[i]){
        return "#"+i+"#";
      }
      else{
        appeared[i] = true;
        ret = "#"+i+"=";
      }
    }

    if(obj instanceof Pair && obj != nil){
      var a = [];
      a.push(to_write_ss(obj.car, cyclic, appeared, array_mode));
      for(var o=obj.cdr; o != nil; o=o.cdr){
        if(!(o instanceof Pair) || cyclic.indexOf(o) >= 0){
          a.push(".");
          a.push(to_write_ss(o, cyclic, appeared, array_mode));
          break;
        }
        a.push(to_write_ss(o.car, cyclic, appeared, array_mode));
      }
      ret += "(" + a.join(" ") + ")";
    }
    else if(obj instanceof Array){
      var a = obj.map(function(item){
        return to_write_ss(item, cyclic, appeared, array_mode);
      })
      if(array_mode)
        ret += "[" + a.join(", ") + "]";
      else
        ret += "#(" + a.join(" ") + ")";
    }
    else{
      ret += to_write(obj);
    }
    return ret;
  }
  BiwaScheme.reduce_cyclic_info = function(known, used){
    var n_used = 0;
    for(var i=0; i<used.length; i++){
      if(used[i]){
        known[n_used] = known[i];
        n_used++;
      }
    }
    return known.slice(0, n_used);
  }
  BiwaScheme.find_cyclic = function(obj, known, used){
    var items = (obj instanceof Pair)  ? [obj.car, obj.cdr] :
                (obj instanceof Array) ? obj :
                null;
    if(!items) return;

    items.each(function(item){
      if(typeof(item)=='number' || typeof(item)=='string' ||
        item === undef || item === true || item === false ||
        item === nil || item instanceof Symbol) return;
      
      var i = known.indexOf(item);
      if(i >= 0)
        used[i] = true;
      else{
        known.push(item);
        used.push(false);
        find_cyclic(item, known, used);
      }
    });
  }

  //
  // variables
  //
  BiwaScheme.TopEnv = {};
  BiwaScheme.CoreEnv = {};

  // (eof-object)
  BiwaScheme.eof = new Object;

  //
  // Set - set of string
  // contents must be string (or at least sortable)
  //
  BiwaScheme.Set = Class.create({
    initialize : function(/*args*/){
      this.arr = [];
      var i;
      for(i=0; i<arguments.length; i++)
        this.arr[i] = arguments[i];
    },

    equals : function(other){
      if(this.arr.length != other.arr.length)
        return false;

      var a1 = this.arr.clone();
      var a2 = other.arr.clone();
      a1.sort();
      a2.sort();
      for(var i=0; i<this.arr.length; i++){
        if(a1[i] != a2[i]) return false;
      }
      return true;
    },
    set_cons : function(item){
      var o = new Set(item);
      o.arr = this.arr.clone();
      o.arr.push(item);
      return o;
    },
    set_union : function(/*args*/){
      var o = new Set();
      o.arr = this.arr.clone();

      for(var k=0; k<arguments.length; k++){
        var s2 = arguments[k];
        if(!s2 instanceof Set) throw new Error("set_union: arguments must be a set");
        for(var i=0; i<s2.arr.length; i++)
          o.add(s2.arr[i]);
      }
      return o;
    },
    set_intersect : function(s2){
      if(!s2 instanceof Set) throw new Error("set_union: arguments must be a set");
      var o = new Set();
      for(var i=0; i<this.arr.length; i++)
        if(s2.member(this.arr[i]))
          o.add(this.arr[i]);
      return o;
    },
    set_minus : function(s2){
      if(!s2 instanceof Set) throw new Error("set_union: arguments must be a set");
      var o = new Set();
      for(var i=0; i<this.arr.length; i++)
        if(!s2.member(this.arr[i]))
          o.add(this.arr[i]);
      return o;
    },
    add : function(item){
      if(!this.member(item)){
        this.arr.push(item);
      }
    },
    member : function(item){
      for(var i=0; i<this.arr.length; i++)
        if(this.arr[i] == item) return true;

      return false;
    },
    rindex : function(item){
      for(var i=this.arr.length-1; i>=0 ; i--)
        if(this.arr[i] == item) return (this.arr.length-1-i);

      return null;
    },
    index : function(item){
      for(var i=0; i<this.arr.length; i++)
        if(this.arr[i] == item) return i;

      return null;
    },
    inspect : function(){
      return "Set(" + this.arr.join(", ") + ")";
    },
    toString : function(){
      return this.inspect();
    },
    size : function(){
      return this.arr.length;
    }
  });

  //
  // Classes
  //

  BiwaScheme.Error = Class.create({
    initialize: function(msg){
      this.message = "Error: "+msg;
    },
    toString: function(){
      return this.message;
    }
  });

  BiwaScheme.Bug = Class.create(Object.extend(new Error(), {
    initialize: function(msg){
      this.message = "[BUG] "+msg;
    }
  }));

  //
  // Pair 
  // cons cell
  //

  BiwaScheme.Pair = Class.create({
    initialize: function(car, cdr){
      this.car = car;
      this.cdr = cdr;
    },

    caar: function(){ return this.car.car; },
    cadr: function(){ return this.cdr.car; },
    cdar: function(){ return this.cdr.car; },
    cddr: function(){ return this.cdr.cdr; },

    first:  function(){ return this.car; },
    second: function(){ return this.cdr.car; },
    third:  function(){ return this.cdr.cdr.car; },
    fourth: function(){ return this.cdr.cdr.cdr.car; },
    fifth:  function(){ return this.cdr.cdr.cdr.cdr.car; },

    // returns array containing all the car's of list
    // '(1 2 3) => [1,2,3]
    // '(1 2 . 3) => [1,2]
    to_array: function(){
      var ary = [];
      for(var o = this; o instanceof Pair && o != nil; o=o.cdr){
        ary.push(o.car);
      }
      return ary;
    },

    to_set: function(){
      var set = new Set();
      for(var o = this; o instanceof Pair && o != nil; o=o.cdr){
        set.add(o.car);
      }
      return set;
    },

    length: function(){
      var n = 0;
      for(var o = this; o instanceof Pair && o != nil; o=o.cdr){
        n++;
      }
      return n;
    },

    // calls the given func passing each car of list
    // returns cdr of last Pair
    foreach: function(func){
      for(var o = this; o instanceof Pair && o != nil; o=o.cdr){
        func(o.car);
      }
      return o;
    },

    // returns human-redable string of pair
    inspect: function(conv){
      conv || (conv = Object.inspect);
      var a = [];
      var last = this.foreach(function(o){
        a.push(conv(o));
      });
      if(last != nil){
        a.push(".");
        a.push(conv(last));
      }
      return "(" + a.join(" ") + ")";
    },
    toString : function(){
      return this.inspect();
    },

    to_write: function(){
      return this.inspect(BiwaScheme.to_write);
    }
  });
  BiwaScheme.List = function(){
    return $A(arguments).to_list();
  }

  //
  // Values
  //
  BiwaScheme.Values = Class.create({
    initialize: function(values){
      this.content = values;
    },
    to_write: function(){
      return "#<Values " + this.content.map(function(x){ return to_write(x) }).join(" ") + ">";
    }
  });

  //
  // Nil
  // javascript representation of empty list( '() )
  //
  BiwaScheme.inner_of_nil = new Object();
  BiwaScheme.inner_of_nil.inspect = function(){
    // Note: should raise error when car of nil is referenced,
    // not when printed
    throw new Error("cannot take car/cdr of '() in Scheme");
  };
  BiwaScheme.nil = new Pair(BiwaScheme.inner_of_nil, 
                            BiwaScheme.inner_of_nil);
  BiwaScheme.nil.toString = function(){ return "nil"; }

  //
  // #<undef> (The undefined value)
  //
  BiwaScheme.undef = new Object();
  BiwaScheme.undef.toString = function(){ return "#<undef>"; }

  //
  // Symbol
  //

  BiwaScheme.Symbol = Class.create({
    initialize: function(str){
      this.name = str;
      Symbols[ str ] = this;
    },

    inspect: function(){
      return "'"+this.name;
      //return "#<Symbol '"+this.name+"'>";
    },

    toString: function(){
      return "'"+this.name;
    },

    to_write: function(){
      return this.name;
    }
  });
  BiwaScheme.Symbols = {};
  BiwaScheme.Sym = function(name,leaveCase){
    if( Symbols[name] === undefined ){
      return new Symbol(name);
    }
    else if( ! (Symbols[name] instanceof Symbol) ){ //pre-defined member (like 'eval' in Firefox)
      return new Symbol(name);
    }
    else{
      return Symbols[name];
    }
  }

  BiwaScheme.gensyms = 0;
  BiwaScheme.gensym = function(){
    BiwaScheme.gensyms++;
    return Sym("__gensym_" + BiwaScheme.gensyms);
  }

  //
  // Char
  //

  BiwaScheme.Char = Class.create({
    initialize: function(c){
      Chars[ this.value = c ] = this;
    },
    to_write: function(){
      switch(this.value){
        case '\n': return "#\\newline";
        case ' ':  return "#\\space";
        case '\t': return "#\\tab";
        default:   return "#\\"+this.value;
      }
    },
    inspect: function(){
      return this.to_write();
    }
  });
  BiwaScheme.Chars = {};
  BiwaScheme.Char.get = function(c) {
    if(typeof(c) != "string") {
      throw new Bug("Char.get: " + Object.inspect(c) + " is not a string");
    }
    if( Chars[c] === undefined )
      return new Char(c);
    else
      return Chars[c];
  }

  //
  // Port
  //
  BiwaScheme.Port = Class.create({
    initialize: function(is_in, is_out){
      this.is_binary = false; //??
      this.is_input = is_in;
      this.is_output = is_out;
    },
    close: function(){
      // close port
    },
    inspect: function(){
      return "#<Port>";
    },
    to_write: function(){
      return "#<Port>";
    }
  });
  BiwaScheme.Port.BrowserInput = Class.create(Port, {
    initialize: function($super){
      $super(true, false);
    },
    get_string: function(after){
      var form = document.createElement("div")
      form.innerHTML = "<input id='webscheme-read-line' type='text'><input id='webscheme-read-line-submit' type='button' value='ok'>";
      $('bs-console').appendChild(form)

      return new BiwaScheme.Pause(function(pause){
        Event.observe($('webscheme-read-line-submit'), 'click', function(){
          var input = $('webscheme-read-line').value;
          form.parentNode.removeChild(form);
          puts(input);
          pause.resume(after(input));
        });
      });
    }
  })
  BiwaScheme.Port.DefaultOutput = Class.create(Port, {
    initialize: function($super){
      $super(false, true);
    },
    put_string: function(str){
      puts(str, true);
    }
  })
  BiwaScheme.Port.current_input  = new Port.BrowserInput();
  BiwaScheme.Port.current_output = new Port.DefaultOutput();
  BiwaScheme.Port.current_error  = new Port.DefaultOutput();

  //
  // Syntax
  //
  BiwaScheme.Syntax = Class.create({
    initialize: function(sname, func){
      this.sname = sname;
      this.func = func;
    },
    transform: function(x){
      if (!this.func){
        throw new Bug("sorry, syntax "+this.sname+" is a pseudo syntax now");
      }
      return this.func(x);
    },
    inspect: function(){
      return "#<Syntax " + this.sname +">";
    }
  })

  // A built-in syntax did not have associated Syntax object.
  // Following code installed dummy Syntax objects to built-in syntax.
  TopEnv["define"] = new Syntax("define");
  TopEnv["begin"]  = new Syntax("begin");
  TopEnv["quote"]  = new Syntax("quote");
  TopEnv["lambda"] = new Syntax("lambda");
  TopEnv["if"]     = new Syntax("if");
  TopEnv["set!"]   = new Syntax("set!");

  //
  // Parser 
  // copied from jsScheme - should be rewrriten (support #0=, etc)
  //
  BiwaScheme.Parser = Class.create({
    initialize: function(txt){
      this.tokens = this.tokenize(txt);
      this.i = 0;
    },

    inspect: function(){
      return [
        "#<Parser:",
        this.i, "/", this.tokens.length, " ",
        Object.inspect(this.tokens),
        ">"
      ].join("");
    },

    tokenize: function(txt) {
      var tokens = new Array(), oldTxt=null;
      var in_srfi_30_comment = 0;

      while( txt != "" && oldTxt != txt ) {
        oldTxt = txt;
        txt = txt.replace( /^\s*(;[^\r\n]*(\r|\n|$)|#;|#\||#\\[^\w]|#?(\(|\[|{)|\)|\]|}|\'|`|,@|,|\+inf\.0|-inf\.0|\+nan\.0|\"(\\(.|$)|[^\"\\])*(\"|$)|[^\s()\[\]{}]+)/,
        function($0,$1) {
          var t = $1;

          if (t == "#|") {
            in_srfi_30_comment++;
            return "";
          }
          else if (in_srfi_30_comment > 0) {
            if ( /(.*\|#)/.test(t) ) {
              in_srfi_30_comment--;
              if (in_srfi_30_comment < 0) {
                throw new Error("Found an extra comment terminator: `|#'")
              }
              // Push back the rest substring to input stream.
              return t.substring(RegExp.$1.length, t.length);
            }
            else {
              return "";
            }
          }
          else {
            if( t.charAt(0) != ';' ) tokens[tokens.length]=t;
            return "";
          }
        } );
      }
      return tokens;
    },

    sexpCommentMarker: new Object,
    getObject: function() {
      var r = this.getObject0();

      if (r != this.sexpCommentMarker)
        return r;

      r = this.getObject();
      if (r == Parser.EOS)
        throw new Error("Readable object not found after S exression comment");

      r = this.getObject();
      return r;
    },
    
    getList: function( close ) {
      var list = nil, prev = list;
      while( this.i < this.tokens.length ) {

        this.eatObjectsInSexpComment("Input stream terminated unexpectedly(in list)");

        if( this.tokens[ this.i ] == ')' || this.tokens[ this.i ] == ']' || this.tokens[ this.i ] == '}' ) {
          this.i++; break;
        }

        if( this.tokens[ this.i ] == '.' ) {
          this.i++;
          var o = this.getObject();
          if( o != Parser.EOS && list != nil ) {
            prev.cdr = o;
          }
        } else {
            var cur = new Pair( this.getObject(), nil);
            if( list == nil ) list = cur;
            else prev.cdr = cur;
            prev = cur;
        }
      }
      return list;
    },

    getVector: function( close ) {
      var arr = new Array();
      while( this.i < this.tokens.length ) {
	
        this.eatObjectsInSexpComment("Input stream terminated unexpectedly(in vector)");
	
        if( this.tokens[ this.i ] == ')' ||
        this.tokens[ this.i ] == ']' ||
        this.tokens[ this.i ] == '}' ) { this.i++; break; }
        arr[ arr.length ] = this.getObject();
      }
      return arr;
    },

    eatObjectsInSexpComment: function(err_msg) {
      while( this.tokens[ this.i ] == '#;' ) {
        this.i++;
        if ((this.getObject() == Parser.EOS) || (this.i >= this.tokens.length))
          throw new Error(err_msg);  
      }
    }, 

    getObject0: function() {
      if( this.i >= this.tokens.length )
        return Parser.EOS;

      var t = this.tokens[ this.i++ ];
      // if( t == ')' ) return null;

      if (t == '#;')
        return this.sexpCommentMarker;

      var s = t == "'"  ? 'quote' :
              t == "`"  ? 'quasiquote' :
              t == ","  ? 'unquote' :
              t == ",@" ? 'unquote-splicing' : false;

      if( s || t == '(' || t == '#(' || t == '[' || t == '#[' || t == '{' || t == '#{' ) {
        return s ? new Pair( Sym(s), new Pair( this.getObject(), nil ))
        : (t=='(' || t=='[' || t=='{') ? this.getList(t) : this.getVector(t);
      } 
      else {
        switch(t){
          case "+inf.0" : return Infinity;
          case "-inf.0" : return -Infinity;
          case "+nan.0" : return NaN;
        }

        var n;
        if( /^#x[0-9a-z]+$/i.test(t) ) {  // #x... Hex
          n = new Number('0x'+t.substring(2,t.length) );
        } 
        else if( /^#d[0-9\.]+$/i.test(t) ) {  // #d... Decimal
          n = new Number( t.substring(2,t.length) );
        } 
        else{
          n = new Number(t);  // use constrictor as parser
        }

        if( ! isNaN(n) ) {
          return n.valueOf();
        } else if( t == '#f' || t == '#F' ) {
          return false;
        } else if( t == '#t' || t == '#T' ) {
          return true;
        } else if( t.toLowerCase() == '#\\newline' ) {
          return Char.get('\n');
        } else if( t.toLowerCase() == '#\\space' ) {
          return Char.get(' ');
        } else if( t.toLowerCase() == '#\\tab' ) {
          return Char.get('\t');
        } else if( /^#\\.$/.test(t) ) {
          return Char.get( t.charAt(2) );
        } else if( /^\"(\\(.|$)|[^\"\\])*\"?$/.test(t) ) {
          return t.replace(/(\r?\n|\\n)/g, "\n").replace( /^\"|\\(.|$)|\"$/g, function($0,$1) {
            return $1 ? $1 : '';
          } );
        } else return Sym(t);  // 2Do: validate !!
      }
    }
  });
  // indicates end of source file
  BiwaScheme.Parser.EOS = new Object();
  
  ///
  /// Compiler
  ///

  BiwaScheme.Compiler = Class.create({
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
      //puts("collect_free "+free.inspect()+" / "+e.inspect()+" => "+opc.inspect());
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
        //puts("compile_refer:"+x.inspect()+" in "+e.inspect()+" results refer-local "+n);
        return return_local(n);
      }
      else if((n = free.index(x)) != null){
        //puts("compile_refer:"+x.inspect()+" in "+e.inspect()+" results refer-free "+n);
        return return_free(n);
      }
      else{
        var sym = x.name;
        return return_global(sym);
      }
      //throw new Error("undefined symbol `" + sym + "'");
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
      while(vars instanceof Pair && vars != nil){
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
      //puts("find_sets: " + to_write(x) + " " + to_write(v))
      var ret=null;
      if(x instanceof Symbol){
        ret = new Set();
      }
      else if(x instanceof Pair){
        switch(x.first()){
        case Sym("define"):
          var exp=x.third();
          ret = this.find_sets(exp, v);
        case Sym("begin"):
          ret = this.find_sets(x.cdr, v); //(ignores improper list)
          break;
        case Sym("quote"):
          ret = new Set();
          break;
        case Sym("lambda"):
          var vars=x.second(), body=x.cdr.cdr;
          ret = this.find_sets(body, v.set_minus(vars.to_set()));
          break;
        case Sym("if"):
          var testc=x.second(), thenc=x.third(), elsec=x.fourth();
          ret = this.find_sets(testc, v).set_union(
                          this.find_sets(thenc, v),
                          this.find_sets(elsec, v));
          break;
        case Sym("set!"):
          var vari=x.second(), xx=x.third();
          if(v.member(vari))
            ret = this.find_sets(xx, v).set_cons(vari);
          else
            ret = this.find_sets(xx, v);
          break;
        case Sym("call/cc"):
          var exp=x.second();
          ret = this.find_sets(exp, v);
          break;
        default:
          var set = new Set();
          for(var p=x; p instanceof Pair && p != nil; p=p.cdr){
            set = set.set_union(this.find_sets(p.car, v));
          }
          ret = set;
          break;
        }
      }
      else{
        ret = new Set();
      }

      if(ret == null)
        throw new Bug("find_sets() exited in unusual way");
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
      if(x instanceof Symbol){
        if(f.member(x))
          ret = new Set(x);
        else
          ret = new Set();
      }
      else if(x instanceof Pair){
        switch(x.first()){
        case Sym("define"):
          var exp=x.third();
          ret = this.find_free(exp, b, f);
          break;
        case Sym("begin"):
          ret = this.find_free(x.cdr, b, f); //(ignores improper list)
          break;
        case Sym("quote"):
          ret = new Set();
          break;
        case Sym("lambda"):
          var vars=x.second(), body=x.cdr.cdr;
          ret = this.find_free(body, b.set_union(vars.to_set()), f);
          break;
        case Sym("if"):
          var testc=x.second(), thenc=x.third(), elsec=x.fourth();
          ret = this.find_free(testc, b, f).set_union(
                          this.find_free(thenc, b, f),
                          this.find_free(elsec, b, f));
          break;
        case Sym("set!"):
          var vari=x.second(), exp=x.third();
          if(f.member(vari))
            ret = this.find_free(exp, b, f).set_cons(vari);
          else
            ret = this.find_free(exp, b, f)
          break;
        case Sym("call/cc"):
          var exp=x.second();
          ret = this.find_free(exp, b, f);
          break;
        default:
          var set = new Set();
          for(var p=x; p instanceof Pair && p != nil; p=p.cdr){
            set = set.set_union(this.find_free(p.car, b, f));
          }
          ret = set;
          break;
        }
      }
      else{
        ret = new Set();
      }
      //p("find_free "+x.inspect()+" / "+b.inspect()+" => "+ret.inspect());

      if(ret == null)
        throw new Bug("find_free() exited in unusual way");
      else
        return ret;
    },

    find_dot_pos: function(x){
      var idx = 0;
      for (; x instanceof Pair && x != nil; x = x.cdr, ++idx)
        ;
      if (x != nil) {
        return idx;
      } else {
        return -1;
      }
    },

    last_pair: function(x){
      if (x instanceof Pair && x != nil){
        for (; x.cdr instanceof Pair && x.cdr != nil; x = x.cdr)
          ;
      }
      return x;
    },

    // dotted list -> proper list
    dotted2proper: function(ls){
      var nreverse = function(ls){
        var res = nil;
        for (; ls instanceof Pair && ls != nil; ){
          var d = ls.cdr;
          ls.cdr = res;
          res = ls;
          ls = d;
        }
        return res;
      }
      var copy_list = function(ls){
        var res = nil;
        for (; ls instanceof Pair && ls != nil; ls = ls.cdr){
          res = new Pair(ls.car, res);
        }
        return nreverse(res);
      }

      if (ls instanceof Pair) {
        var last = this.last_pair(ls);
        if (last instanceof Pair && last.cdr == nil){
          return ls;
        } else {
          var copied = copy_list(ls);
          this.last_pair(copied).cdr = new Pair(last.cdr, nil);
          return copied;
        }
      } else {
        return new Pair(ls, nil);
      }
    },

    // x: exp(list of symbol or integer or..)
    // e: env (= [locals, frees])
    // s: vars might be set!
    // next: opc
    // ret: opc
    compile: function(x, e, s, f, next){
      //p(x);
      var ret = null;

      while(1){
        if(x instanceof Symbol){
          return this.compile_refer(x, e, (s.member(x) ? ["indirect", next] : next));
        }
        else if(x instanceof Pair){
          switch(x.first()){
          case Sym("define"):
            var left = x.cdr.car;
            var exp  = x.cdr.cdr;
            
            //define variable
            if(left instanceof Symbol){    
              x = exp.car;
              TopEnv[left.name] = BiwaScheme.undef;
              next = ["assign-global", left.name, next]; //should raise for improper list?
            }
            //define function 
            else if(left instanceof Pair){ 
              var fname=left.car, args=left.cdr;
              var lambda = new Pair(Sym("lambda"), new Pair(args, exp));
              x = lambda;
              TopEnv[fname.name] = BiwaScheme.undef;
              next = ["assign-global", fname.name, next];
            }
            //error
            else{                          
              throw new Error("compile: define needs a leftbol or pair: got "+left);
            }
            break;
          case Sym("begin"):
            var a = [];
            for(var p=x.cdr; p instanceof Pair && p!=nil; p=p.cdr)
              a.push(p.car);

            //compile each expression (in reverse order)
            var c = next;
            for(var i=a.length-1; i>=0; i--){
              c = this.compile(a[i], e, s, f, c);
            }
            return c;
          case Sym("quote"):
            var obj=x.second();
            return ["constant", obj, next];
          case Sym("lambda"):
            // x = '(lambda (x y) x y)
            // x = '(lambda vars x y)
            var vars = x.cdr.car;
            var body = new Pair(Sym("begin"), x.cdr.cdr); //tenuki

            var dotpos = this.find_dot_pos(vars);
            var proper = this.dotted2proper(vars);
            var free = this.find_free(body, proper.to_set(), f); //free variables
            var sets = this.find_sets(body, proper.to_set()); //local variables

            var do_body = this.compile(body,
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
          case Sym("if"):
            var testc=x.second(), thenc=x.third(), elsec=x.fourth();
            var thenc = this.compile(thenc, e, s, f, next);
            var elsec = this.compile(elsec, e, s, f, next);
            x    = testc;
            next = ["test", thenc, elsec];
            break;
          case Sym("set!"):
            var v=x.second(), x=x.third();
            var do_assign = this.compile_lookup(v, e,
              function(n){ return ["assign-local", n, next]; },
              function(n){ return ["assign-free",  n, next]; },
              function(sym){ return ["assign-global",sym, next]; }
            );
            next = do_assign;
            break;
          case Sym("call/cc"): 
            var x=x.second();
            var c = ["conti", 
                      (this.is_tail(next) ? (e[0].size() + 1) : 0), //number of args for outer lambda
                      ["argument",
                      ["constant", 1,
                      ["argument",
                        this.compile(x, e, s,f,  
                          (this.is_tail(next) ? ["shift", 1, ["apply"]]
                                              : ["apply"]))]]]];
                    //note: proc for call/cc takes 1 argument (= ["apply", 1])
            return this.is_tail(next) ? c : ["frame", c, next];
          default: 
            //apply 
            //x = (func 1 2) 
            //x.car = func = '(lambda (x) ..) or Symbol
            //x.cdr = args = '(1 2)
            var func = x.car;
            var args = x.cdr;
            var c = this.compile(func, e, s,f,  
                      this.is_tail(next) ? ["shift", args.length(), ["apply"]]
                                         : ["apply"]);

	    // VM will push the number of arguments to the stack.
            c = this.compile(args.length(), e, s, f, ["argument", c]);
            for(var p=args; p instanceof Pair && p!=nil; p=p.cdr){
              c = this.compile(p.car, e, s, f, ["argument", c]);
            }
            return this.is_tail(next) ? c : ["frame", c, next];
          }
        }
        else{
          return ["constant", x, next];
        }
      }
      //p("result of " + x.inspect() + ":");
      //p(ret);
      //dump(new Hash({"ret":ret, "x":x, "e":e, "s":s, "next":next, "stack":[]}));
//      if(ret == null)
//        throw new Bug("compile() exited in unusual way");
//      else
//        return ret;
    },
    run: function(expr){
      return this.compile(expr, [new Set(), new Set()], new Set(), new Set(), ["halt"]);
    }
  });
  BiwaScheme.Compiler.compile = function(expr, next){
    expr = (new Interpreter).expand(expr);
    return (new Compiler).run(expr, next);
  }

  ///
  /// Interpreter
  ///

  //
  // pause object (facility to stop/resume interpreting)
  //
  BiwaScheme.Pause = Class.create({
    //new (on_pause: javascript function calling setTimeout, Ajax.Request, ..)
    initialize: function(on_pause){
      this.on_pause = on_pause;
    },

    //save state of interpreter
    set_state: function(intp, x, f, c, s){
      this.interpreter = intp;
      this.x = x;
      this.f = f;
      this.c = c;
      this.s = s;
    },

    //call this when ready (to fire setTimeout, Ajax.Request..)
    ready: function(){
      this.on_pause(this);
    },

    //restart calculation
    resume: function(value){
      return this.interpreter.resume(true, value, this.x, this.f, this.c, this.s)
    }
  });

  BiwaScheme.Call = Class.create({
    initialize: function(proc, args, after){
      this.proc = proc;
      this.args = args;
      this.after = after || function(ar){
        // just return result which closure returned
        return ar[0];
      };
    }
  })
  
  BiwaScheme.Interpreter = Class.create({
    initialize: function(on_error){
      this.stack = [] //(make-vector 1000)
      this.on_error = on_error || function(e){};
      this.after_evaluate = Prototype.emptyFunction;
    },

    inspect: function(){
      return [
        "#<Interpreter: stack size=>",
        this.stack.length, " ",
        "after_evaluate=",
        Object.inspect(this.after_evaluate),
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
      return v;
    },

    //v: stack array to restore
    //ret: lenght of restored stack
    restore_stack: function(v){
      var s = v.length;
      for(var i=0; i<s; i++){
        this.stack[i] = v[i];
      }
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
        return this.on_error(e);
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
	var state = new Hash({"a":a, 
			      "f":f, 
			      "c":c, 
			      "s":s, 
			      "x":x, 
			      "stack":this.stack});
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
          break;
        case "refer-free":
          var n=x[1], x=x[2];
          a = c[n+1];
          break;
        case "refer-global":
          var sym=x[1], x=x[2];
          if(TopEnv.hasOwnProperty(sym)) var val = TopEnv[sym];
          else if(CoreEnv.hasOwnProperty(sym)) var val = CoreEnv[sym];
          else throw new Error("execute: unbound symbol: "+Object.inspect(sym));

          a = val;
          break;
        case "indirect":
          var x=x[1];
          a = a[0]; //unboxing
          break;
        case "constant":
          var obj=x[1], x=x[2];
          a = obj;
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
          if(!TopEnv.hasOwnProperty(name))
            throw new Error("global variable '"+name+"' is not defined");
          
          TopEnv[name] = a;
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
        case "apply": //extended: n_args as second argument
          var func = a; //, n_args = x[1];

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
              var ls = nil;
              for (var i=n_args; --i>=dotpos; ) {
                ls = new Pair(this.index(s, i), ls);
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
          else if(func instanceof Function){
            var args = [];
            for(var i=0; i<n_args; i++) 
              args.push(this.index(s, i));

            var result = func(args, this);

            if(result instanceof Pause){
              var pause = result;
              pause.set_state(this, ["return"], f, c, s);
              pause.ready();
              return pause;
            }
            else if(result instanceof Call){
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
              var push_args = result.args.inject(call_proc, function(opc, arg){
                // (foo 1 2) => first push 2, then 1
                //   [constant 2 ... [constant 1 ... ]
                return ["constant", arg, 
                       ["argument",
                       opc]];
              })
              x = ["frame",
                    push_args,
                  call_after]
            }
            else{
              a = result;
              x = ["return"];
            }
          }
          else{
            throw new Error("execute: unknown function type: "+Object.inspect(func));
          }
          break;
        case "return":
          var n=this.index(s, -1);
          var ss=s-n;
          x = this.index(ss, 0),
          f = this.index(ss, 1),
          c = this.index(ss, 2),
          s = ss-3-1;
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
    },

    // expand macro forms (recursively)
    expand: function(x, flag){
      flag || (flag = {})
      var ret = null;
      if(x instanceof Symbol){
        ret = x;
      }
      else if(x instanceof Pair){
        switch(x.car){
        case Sym("define"):
          var left = x.cdr.car, exp = x.cdr.cdr;
          ret = new Pair(Sym("define"),
                  new Pair(left, this.expand(exp, flag)));
          break;
        case Sym("begin"):
          ret = new Pair(Sym("begin"), this.expand(x.cdr, flag));
          break;
        case Sym("quote"):
          ret = x;
          break;
        case Sym("lambda"):
          var vars=x.cdr.car, body=x.cdr.cdr;
          ret = new Pair(Sym("lambda"),
                  new Pair(vars, this.expand(body, flag)));
          break;
        case Sym("if"):
          var testc=x.second(), thenc=x.third(), elsec=x.fourth();
          if (elsec == BiwaScheme.inner_of_nil){
            elsec = BiwaScheme.undef;
          }
          ret = [
            Sym("if"), 
            this.expand(testc, flag), 
            this.expand(thenc, flag),
            this.expand(elsec, flag)
          ].to_list();
          break;
        case Sym("set!"):
          var v=x.second(), x=x.third();
          ret = [Sym("set!"), v, this.expand(x, flag)].to_list();
          break;
        case Sym("call-with-current-continuation"): 
        case Sym("call/cc"): 
          var x=x.second();
          ret = [Sym("call/cc"), this.expand(x, flag)].to_list();
          break;
        default: //apply
          // if x is a macro call ...
          if(x.car instanceof Symbol && TopEnv[x.car.name] instanceof Syntax){
            var transformer = TopEnv[x.car.name];
            flag["modified"] = true;
            ret = transformer.transform(x);

            if(BiwaScheme.Debug){
              var before = to_write(x), after = to_write(ret);
              if(before != after) puts("expand: " + before + " => " + after)
            }

            var fl;
            for(;;){
              ret = this.expand(ret, fl={});
              if(!fl["modified"]) 
                break;
            }
          }
          else if(x == nil)
            ret = nil;
          else{
            ret = new Pair(this.expand(x.car, flag), x.cdr.to_array().map(function(item){return this.expand(item, flag)}.bind(this)).to_list());
          }
        }
      }
      else{
        ret = x;
      }
      return ret;
    },

    evaluate: function(str, after_evaluate){
      this.parser = new Parser(str);
      this.compiler = new Compiler();
      if(after_evaluate) 
        this.after_evaluate = after_evaluate;

      if(BiwaScheme.Debug) puts("executing: " + str);
       
      this.is_top = true;
      this.file_stack = [];
      return this.resume(false);
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
          if(expr === Parser.EOS) break;

          // expand
          expr = this.expand(expr);

          // compile
          var opc = this.compiler.run(expr);
          //if(BiwaScheme.Debug) p(opc);

          // execute
          ret = this.execute(expr, opc, 0, [], 0);
        }

        if(ret instanceof Pause){ //suspend evaluation
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
      var obj = new Parser(str).getObject();
      var opc = Compiler.compile(obj);
      return opc;
    }
  });
  BiwaScheme.Interpreter.read = function(str){
    var parser = new Parser(str);
    return parser.getObject();
  }

  /* --------------------------------------- namespace webscheme */
}
