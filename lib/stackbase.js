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
      return "#<undef>";
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
        item === undefined || item === true || item === false ||
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
  // Dumper - graphical state dumper
  //

  BiwaScheme.Dumper = Class.create({
    initialize: function(){
    },

    is_opc: function(obj){
      return (obj instanceof Array && typeof(obj[0]) == 'string');
    },

    dump_pad: "&nbsp;&nbsp;&nbsp;",
    dump_opc: function(obj, level){
      var s="";
      var pad1="", pad2="";
      var level = level || 0;
      level.times(function(){ pad1 += this.dump_pad; }.bind(this));
      (level+1).times(function(){ pad2 += this.dump_pad; }.bind(this));

      s += pad1 + '[<span class="dump_opecode">' + obj[0] + '</span>';
      var i = 1;
      while(!(obj[i] instanceof Array) && i<obj.length){
        if(obj[0] == "constant")
          s += "&nbsp;<span class='dump_constant'>" + this.dump_obj(obj[i]) + "</span>";
        else
          s += "&nbsp;" + this.dump_obj(obj[i]);
        i++;
      }
      if(i < obj.length) s += '<br>\n';
      for(; i<obj.length; i++){
        if(this.is_opc(obj[i])){
          s += this.dump_opc(obj[i], (i == obj.length-1 ? level : level+1));
        }
        else{
          s += (i == obj.length-1) ? pad1 : pad2;
          s += this.dump_obj(obj[i]); //String(obj[i]).escapeHTML();
        }
        if(i != obj.length-1) s += "<br>\n";
      }
      s += "]";
      return (level==0 ? this.add_fold(s) : s);
    },

    fold_limit: 20,
    n_folds:  0,
    add_fold: function(s){
      var lines = s.split(/<br>/gmi);

      if(lines.length > this.fold_limit){
        var fold_btn   = " <span style='text-decoration:underline; color:blue; cursor:pointer;'" +
                             "onclick='BiwaScheme.Dumper.toggle_fold("+this.n_folds+")'>more</span>"
        var fold_start = "<div style='display:none' id='fold"+this.n_folds+"'>"; 
        var fold_end   = "</div>"
        this.n_folds++;
        return lines.slice(0, this.fold_limit).join("<br>") + fold_btn +
                 fold_start + lines.slice(this.fold_limit+1).join("<br>") + fold_end;
      }
      else{
        return s;
      }
    },

    stack_max_len: 80,
    dump_stack: function(stk, size){
      if(stk == null || stk == undefined) return Object.inspect(stk)
      var s = "<table>";
      for(var i=stk.length-1; i >= 0; i--){
        if(i < size){
          s += "<tr><td class='dump_stknum'>[" + i + "]</td>" +
               "<td>" + this.dump_obj(stk[i]).truncate(this.stack_max_len) + "</td></tr>";
        }
        else{
          s += "<tr><td class='dump_dead'>[" + i + "]</td>" +
               "<td class='dump_dead'>" + this.dump_obj(stk[i]).truncate(this.stack_max_len) + "</td></tr>";
        }
      }
      return s + "</table>";
    },

    dump_object: function(obj){
      var a = [];
      for(var k in obj){
        //if(this.prototype[k]) continue;
        a.push( k.toString() );//+" => "+this[k].toString() );
      }
      return "#<Object{"+a.join(",")+"}>";
    },

    closures: [],
    dump_closure: function(cls){
      if(cls.length == 0) return "[]";

      var cls_num = null;
      for(var i=0; i<this.closures.length; i++){
        if(this.closures[i] == cls) cls_num = i;
      }
      if(cls_num == null){
        cls_num = this.closures.length;
        this.closures.push(cls);
      }

      var c = cls.clone ? cls.clone() : [c];
      var body = c.shift();
      return "c"+cls_num+" <span class='dump_closure'>free vars :</span> " + this.dump_obj(c) + " <span class='dump_closure'>body :</span> " + this.dump_obj(body).truncate(100);
    },

    dump_obj: function(obj){
      if(obj && typeof(obj.to_html) == 'function')
        return obj.to_html();
      else{
        var s = write_ss(obj, true); //true=Array mode
        if(s == "[object Object]") s = this.dump_object(obj);
        return s.escapeHTML();
      }
    },

    n_dumps: 0,
    dump: function(obj){
      var dumpitem = document.createElement("div"); 
      var s = "";
      if(obj instanceof Hash){
        s += "<table>"
        s += "<tr><td colspan='4'>#"+this.n_dumps+"</td></tr>"
        obj.each(function(pair){
          if(pair.key!="x" && pair.key != "stack"){
            var value = (pair.key=="c" ? this.dump_closure(pair.value) : this.dump_obj(pair.value))
            s += "<tr><td>" + pair.key + ": </td><td colspan='3'>" + value + "</td></tr>";
          }
        }.bind(this));
        s += "<tr><td>x:</td><td>" + (this.is_opc(obj.get("x")) ? this.dump_opc(obj.get("x")) : this.dump_obj(obj.get("x"))) + "</td>";
        s +=     "<td style='border-left: 1px solid black'>stack:</td><td>" + this.dump_stack(obj.get("stack"), obj.get("s")) + "</td></tr>";
        s += "</table>";
      }
      else{
        s = Object.inspect(obj).escapeHTML() + "<br>\n";
      }
      dumpitem.id = "dump" + this.n_dumps;
      dumpitem.innerHTML = s;
      $('dumparea').appendChild(dumpitem);
      Element.hide(dumpitem);
      this.n_dumps++;
    },

    cur: -1,
    dump_move: function(dir){
      if(0 <= this.cur && this.cur < this.n_dumps)
        Element.hide($("dump"+this.cur));

      if(0 <= this.cur+dir && this.cur+dir < this.n_dumps)
        this.cur += dir;

      Element.show($("dump"+this.cur));
    },

    is_folded: true,
    dump_toggle_fold: function(){
      if(this.is_folded){ //open all
        for(var i=0; i<this.n_dumps; i++)
          Element.show($("dump"+i));
      }
      else{            //close all
        for(var i=0; i<this.n_dumps; i++)
          if(i!=this.cur) Element.hide($("dump"+i));
      }
      this.is_folded = (!this.is_folded);
    }
  })
  BiwaScheme.Dumper.toggle_fold = function(n){
    Element.toggle("fold"+n);
  }


  //
  // Nil
  // javascript representation of empty list( '() )
  //
  BiwaScheme.nil = new Pair(null, null);
  BiwaScheme.nil.toString = function(){ return "nil"; }

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
    if( Symbols[name] == undefined ){
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
        default:   return "#\\"+this.value;
      }
    },
    inspect: function(){
      return this.to_write();
    }
  });
  BiwaScheme.Chars = {};
  BiwaScheme.Char.get = function(c) {
    if( Chars[c] != undefined ) {
      return Chars[c];
    }
    return new Char(c);
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
    initialize: function(func){
      this.func = func;
    },
    transform: function(x){
      return this.func(x);
    }
  })

  //
  // Parser 
  // copied from jsScheme - should be rewrriten (support #0=, etc)
  //
  BiwaScheme.Parser = Class.create({
    initialize: function(txt){
      this.tokens = this.tokenize(txt);
      this.i = 0;
    },

    tokenize: function(txt) {
      var tokens = new Array(), oldTxt=null;
      while( txt != "" && oldTxt != txt ) {
        oldTxt = txt;
        txt = txt.replace( /^\s*(;[^\r\n]*(\r|\n|$)|#\\[^\w]|#?(\(|\[|{)|\)|\]|}|\'|`|,@|,|\+inf\.0|-inf\.0|\+nan\.0|\"(\\(.|$)|[^\"\\])*(\"|$)|[^\s()\[\]{}]+)/,
        function($0,$1) {
          if( $1.charAt(0) != ';' ) tokens[tokens.length]=$1;
          return "";
        } );
      }
      return tokens;
    },

    getList: function( close ) {
      var list = nil, prev = list;
      while( this.i < this.tokens.length ) {
        if( this.tokens[ this.i ] == ')' || this.tokens[ this.i ] == ']' || this.tokens[ this.i ] == '}' ) {
          this.i++; break;
        }

        if( this.tokens[ this.i ] == '.' ) {
          this.i++;
          var o = this.getObject();
          if( o != null && list != nil ) {
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
        if( this.tokens[ this.i ] == ')' ||
        this.tokens[ this.i ] == ']' ||
        this.tokens[ this.i ] == '}' ) { this.i++; break; }
        arr[ arr.length ] = this.getObject();
      }
      return arr;
    },

    getObject: function() {
      if( this.i >= this.tokens.length ) return null;
      var t = this.tokens[ this.i++ ];
      // if( t == ')' ) return null;

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
          var vars=x.second(), body=x.third();
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
          var vars=x.second(), body=x.third();
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
              TopEnv[left.name] = undefined;
              next = ["assign-global", left.name, next]; //should raise for improper list?
            }
            //define function 
            else if(left instanceof Pair){ 
              var fname=left.car, args=left.cdr;
              var lambda = new Pair(Sym("lambda"), new Pair(args, exp));
              x = lambda;
              TopEnv[fname.name] = undefined;
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

            var free = this.find_free(body, vars.to_set(), f); //free variables
            var sets = this.find_sets(body, vars.to_set()); //local variables

            var do_body = this.compile(body,
                            [vars.to_set(), free],
                            sets.set_union(s.set_intersect(free)),
                            f.set_union(vars.to_set()),
                            ["return", vars.length()]);
            var do_close = ["close", 
                             free.size(),
                             this.make_boxes(sets, vars, do_body),
                             next];
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
              function(sym){ return ["assign-global",sym, next]; });
            next = do_assign;
            break;
          case Sym("call/cc"): 
            var x=x.second();
            var c = ["conti", 
                      (this.is_tail(next) ? next[1] : 0), //number of args for outer lambda
                      ["argument",
                        this.compile(x, e, s,f,  
                          (this.is_tail(next) ? ["shift", 1, next[1], ["apply", 1]]
                                              : ["apply", 1]))]];
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
                      this.is_tail(next) ? ["shift", args.length(), next[1], ["apply", args.length()]]
                                         : ["apply", args.length()]);
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
      return this.closure(["refer-local", 0,
                            ["nuate", this.save_stack(s), 
                            ["return", n]]], 
                          0,     //n (number of frees)
                          null); //s (stack position to get frees)
    },

    // shift stack 
    // n: number of items to skip (from stack top)
    // m: number of items to shift
    // s: stack pointer (= index of stack top + 1)
    shift_args: function(n, m, s){
      for(var i = n-1; i >= 0; i--){
        this.index_set(s, i+m, this.index(s, i));
      }
      return s-m;
    },

    index: function(s, i){
      return this.stack[s-i-1];
    },

    index_set: function(s, i, v){
      this.stack[s-i-1] = v;
    },

    //ret: [body, stack[s-1], stack[s-2], ..]
    closure: function(body, n, s){
      var v = []; //(make-vector n+1)
      v[0] = body;
      for(var i=0; i<n; i++)
        v[i+1] = this.index(s, i);
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

    _execute: function(a, x, f, c, s){
      var ret = null;
      //puts("executing "+x[0]);
      
      while(true){ //x[0] != "halt"){
        dump(new Hash({"a":a, "f":f, "c":c, "s":s, "x":x, "stack":this.stack}))
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
          var n=x[1], body=x[2], x=x[3];
          a = this.closure(body, n, s);
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
          a = name;
          break;
        case "assign-local":
          var n=x[1], x=x[2];
          var box = this.index(f, n);
          box[0] = a;
          break;
        case "assign-free":
          var n=x[1], x=x[2];
          var box = c[n+1];
          box[0] = a;
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
          var n=x[1], m=x[2], x=x[3];
          s = this.shift_args(n, m, s);
          break;
        case "apply": //extended: n_args as second argument
          var func = a, n_args = x[1];
          if(func instanceof Array){ //closure
            a = func;
            x = func[0];
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
              pause.set_state(this, ["return", n_args], f, c, s);
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
                                 ["constant", result.after,
                                 ["apply", 1]]],
                               ["return", n_args]];
              var call_proc = ["constant", result.proc, 
                              ["apply", result.args.length]];
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
              x = ["return", n_args];
            }
          }
          else{
            throw new Error("execute: unknown function type: "+Object.inspect(func));
          }
          break;
        case "return":
          var n=x[1];
          var ss=s-n; 
          x = this.index(ss, 0),
          f = this.index(ss, 1),
          c = this.index(ss, 2),
          s = ss-3;
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
          ret = [Sym("if"), this.expand(testc, flag), this.expand(thenc, flag), this.expand(elsec, flag)].to_list();
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
      this.after_evaluate = after_evaluate || function(ret){};

      if(BiwaScheme.Debug) puts("executing: " + str);
       
      this.is_top = true;
      this.file_stack = [];
      return this.resume(false);
    },

    resume: function(is_resume, a, x, f, c, s){
      var ret = undefined;

      for(;;){
        if(is_resume){
          ret = this.execute(a, x, f, c, s);
          is_resume = false;
        }
        else{
          if(!this.parser) break; // adhoc: when Pause is used via invoke_closure
          var expr = this.parser.getObject();
          if(!expr) break;
//          if(this.is_top){
//            if(expr instanceof Pair && expr.car == Sym("load")){
//            if(expr.car == Sym("load")){
//              this.load(expr);
//              continue;
//            }
//            else
//              this.is_top = false
//          }

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

      var x = ["constant", closure, ["apply", n_args]]
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
