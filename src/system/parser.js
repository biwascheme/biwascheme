  //
  // Parser 
  // copied from jsScheme - should be rewrriten (support #0=, etc)
  //
  BiwaScheme.Parser = BiwaScheme.Class.create({
    initialize: function(txt){
      this.tokens = this.tokenize(txt);
      this.i = 0;
    },

    inspect: function(){
      return [
        "#<Parser:",
        this.i, "/", this.tokens.length, " ",
        BiwaScheme.inspect(this.tokens),
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
                throw new BiwaScheme.Error("Found an extra comment terminator: `|#'")
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
      if (r == BiwaScheme.Parser.EOS)
        throw new BiwaScheme.Error("Readable object not found after S exression comment");

      r = this.getObject();
      return r;
    },
    
    getList: function( close ) {
      var list = BiwaScheme.nil, prev = list;
      while( this.i < this.tokens.length ) {

        this.eatObjectsInSexpComment("Input stream terminated unexpectedly(in list)");

        if( this.tokens[ this.i ] == ')' || this.tokens[ this.i ] == ']' || this.tokens[ this.i ] == '}' ) {
          this.i++; break;
        }

        if( this.tokens[ this.i ] == '.' ) {
          this.i++;
          var o = this.getObject();
          if( o != BiwaScheme.Parser.EOS && list != BiwaScheme.nil ) {
            prev.cdr = o;
          }
        } else {
            var cur = new BiwaScheme.Pair( this.getObject(), BiwaScheme.nil);
            if( list == BiwaScheme.nil ) list = cur;
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
        if ((this.getObject() == BiwaScheme.Parser.EOS) || (this.i >= this.tokens.length))
          throw new BiwaScheme.Error(err_msg);  
      }
    }, 

    getObject0: function() {
      if( this.i >= this.tokens.length )
        return BiwaScheme.Parser.EOS;

      var t = this.tokens[ this.i++ ];
      // if( t == ')' ) return null;

      if (t == '#;')
        return this.sexpCommentMarker;

      var s = t == "'"  ? 'quote' :
              t == "`"  ? 'quasiquote' :
              t == ","  ? 'unquote' :
              t == ",@" ? 'unquote-splicing' : false;

      if( s || t == '(' || t == '#(' || t == '[' || t == '#[' || t == '{' || t == '#{' ) {
        return s ? new BiwaScheme.Pair( BiwaScheme.Sym(s), new BiwaScheme.Pair( this.getObject(), BiwaScheme.nil ))
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
          return BiwaScheme.Char.get('\n');
        } else if( t.toLowerCase() == '#\\space' ) {
          return BiwaScheme.Char.get(' ');
        } else if( t.toLowerCase() == '#\\tab' ) {
          return BiwaScheme.Char.get('\t');
        } else if( /^#\\.$/.test(t) ) {
          return BiwaScheme.Char.get( t.charAt(2) );
        } else if( /^#\\x[a-zA-Z0-9]+$/.test(t) ) {
          var scalar = parseInt(t.slice(3), 16);
          // R6RS 11.11 (surrogate codepoints)
          if (scalar >= 0xD800 && scalar <= 0xDFFF) {
            throw new BiwaScheme.Error("Character in Unicode excluded range.");
          }
          // ECMA-262 4.3.16 -- Basically, strings are sequences of 16-bit
          // unsigned integers, so anything greater than 0xFFFF won't fit.
          // NOTE: This violates R6RS which requires the full Unicode range!
          else if (scalar > 0xFFFF) {
            throw new BiwaScheme.Error("Character literal out of range.");
          } else {
            return BiwaScheme.Char.get(String.fromCharCode(scalar));
          }
        } else if( /^\"(\\(.|$)|[^\"\\])*\"?$/.test(t) ) {
          return t.replace(/(\r?\n|\\n)/g, "\n").replace( /^\"|\\(.|$)|\"$/g, function($0,$1) {
            return $1 ? $1 : '';
          } );
        } else return BiwaScheme.Sym(t);  // 2Do: validate !!
      }
    }
  });
  // indicates end of source file
  BiwaScheme.Parser.EOS = new Object();
  

