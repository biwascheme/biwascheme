import { nil } from "../header.js"
import { inspect } from "./_writer.js"
import { Char } from "./char.js"
import { BiwaError } from "./error.js"
import { Pair } from "./pair.js"
import { Sym } from "./symbol.js"

//
// Parser 
// copied from jsScheme - should be rewrriten (support #0=, etc)
//
class Parser {
  constructor(txt){
    this.tokens = this.tokenize(txt);
    this.i = 0;
    this.sexpCommentMarker = new Object();
  }

  // Inject scheme program into current position
  insert(txt) {
    this.tokens.splice(this.i, 0, ...this.tokenize(txt));
  }

  // Returns string representation of `this` for debugging
  inspect(){
    return [
      "#<Parser:",
      this.i, "/", this.tokens.length, " ",
      inspect(this.tokens),
      ">"
    ].join("");
  }

  // Split a string into array of tokens
  tokenize(txt) {
    var tokens = new Array(), oldTxt=null;
    var in_srfi_30_comment = 0;

    while( txt != "" && oldTxt != txt ) {
      oldTxt = txt;
      txt = txt.replace( /^\s*(;[^\r\n]*(\r|\n|$)|#;|#\||#\\[^\w]|#?(\(|\[|{)|\)|\]|}|\'|`|,@|,|\+inf\.0|-inf\.0|\+nan\.0|\"(\\(.|$)|[^\"\\])*(\"|$)|\|(\\(.|$)|[^\|\\])*(\||$)|[^\s()\[\]{}]+)/,
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
              throw new BiwaError("Found an extra comment terminator: `|#'")
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
  }

  // Read a Scheme object. Skip sexp comment if any
  getObject() {
    var r = this.getObject0();

    if (r != this.sexpCommentMarker)
      return r;

    r = this.getObject();
    if (r == Parser.EOS)
      throw new BiwaError("Readable object not found after S exression comment");

    r = this.getObject();
    return r;
  }
  
  // Read a Scheme list
  getList( close ) {
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
  }

  // Read a Scheme vector
  getVector( close ) {
    var arr = new Array();
    while( this.i < this.tokens.length ) {
      
      this.eatObjectsInSexpComment("Input stream terminated unexpectedly(in vector)");
      
      if( this.tokens[ this.i ] == ')' ||
      this.tokens[ this.i ] == ']' ||
      this.tokens[ this.i ] == '}' ) { this.i++; break; }
      arr[ arr.length ] = this.getObject();
    }
    return arr;
  }

  // Skip contents of sexp comment
  eatObjectsInSexpComment(err_msg) {
    while( this.tokens[ this.i ] == '#;' ) {
      this.i++;
      if ((this.getObject() == Parser.EOS) || (this.i >= this.tokens.length))
        throw new BiwaError(err_msg);  
    }
  } 

  // Read a Scheme object
  // Returns `sexpCommentMarker` if `#;` is found
  getObject0() {
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
      return s ? new Pair(Sym(s), new Pair( this.getObject(), nil ))
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
      else if( /^#o[0-9\.]+$/i.test(t) ) {  // #o... Octal
        n = new Number(parseInt('0'+t.substring(2,t.length),8));
      }
      else if( /^#b[0-9\.]+$/i.test(t) ) {  // #b... Binary
        n = new Number(parseInt(t.substring(2,t.length),2));
      }
      else{
        n = new Number(t);  // use constructor as parser
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
      } else if( /^#\\x[a-zA-Z0-9]+$/.test(t) ) {
        var scalar = parseInt(t.slice(3), 16);
        // R6RS 11.11 (surrogate codepoints)
        if (scalar >= 0xD800 && scalar <= 0xDFFF) {
          throw new BiwaError("Character in Unicode excluded range.");
        }
        // ECMA-262 4.3.16 -- Basically, strings are sequences of 16-bit
        // unsigned integers, so anything greater than 0xFFFF won't fit.
        // NOTE: This violates R6RS which requires the full Unicode range!
        else if (scalar > 0xFFFF) {
          throw new BiwaError("Character literal out of range.");
        } else {
          return Char.get(String.fromCharCode(scalar));
        }
      } else if( /^\"(\\(.|$)|[^\"\\])*\"?$/.test(t) ) {
        // String literal
        
        return t.replace(/(\r|\n|(?:^|[^\\+])\\(\\\\)*n)/g, function(match){
          return match.replace(/(\r?\n|\\n)/g, "\n");
        }).replace( /^\"|\\(.|$)|\"$/g, function($0,$1) {
          return $1 ? $1 : '';
        } );
      } else if( /^\|[^\|]*\|/.test(t) ) {
        // Symbol literal with vertical bars (eg. `|a b|`)
        const s = t.replace( /^\|/, "" )
                   .replace( /\|$/, "" );
        return Sym(s);
      } else {
        return Sym(t);
      }
    }
  }
}

// indicates end of source file
Parser.EOS = new Object();

// Parser the text and return an array of exprs
Parser.parse = txt => {
  const parser = new Parser(txt);
  const ret = [];
  while (true) {
    var expr = parser.getObject();
    if (expr === Parser.EOS) break;
    ret.push(expr);
  }
  return ret;
};
  
export default Parser;
