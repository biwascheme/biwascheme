import { nil } from "../header.js"
import { inspect } from "./_writer.js"
import { Char } from "./char.js"
import { BiwaError, Bug } from "./error.js"
import { Pair, List } from "./pair.js"
import { Sym } from "./symbol.js"
import { isSymbol, isString } from "./_types.js"
import { Complex } from "./number.js"

// Allowed digits in each base
const DIGITS = {
  2: /^[01]+/,
  8: /^[0-7]+/,
  10: /^[0-9]+/,
  16: /^[0-9a-fA-F]+/,
};

// Matching parenthesis/bracket for lists
const PAREN = {
  "(": ")",
  "{": "}",
  "[": "]",
}

// Named characters like `#\newline`
const NAMED_CHARS = {
  alarm: "\x07",
  backspace: "\x08",
  delete: "\x7F",
  escape: "\x1B",
  newline: "\n",
  null: "\x00",
  return: "\x0D",
  space: " ",
  tab: "\t",
}
const NAMED_CHARS_REXP = new RegExp(
  "^(" + 
  Object.keys(NAMED_CHARS).join("|") + 
  ")\\b");

// Escape sequences like `\n`, `\t`
const ESCAPE_SEQUENCES = {
  a: "\x07",
  b: "\x08",
  t: "\t",
  n: "\n",
  r: "\x0D",
}

class Parser {
  constructor(txt) {
    // Scheme source text
    this.txt = txt;
    // Current position
    this.i = 0;
    // Case fold flag. Changed by `#!(no-)fold-case` directives in Scheme source text
    this.foldCase = false;
    // For datum labels (`#1=`)
    this.labelledData = [];
  }

  // Inject scheme program into current position
  insert(txt) {
    const orig = this.txt;
    this.txt = orig.slice(0, this.i) + txt + orig.slice(this.i);
  }

  // Returns string representation of `this` for debugging
  inspect(){
    return `#<Parser (${this.i}/${this.txt.length})>`;
  }

  // Read a Scheme value and returns it. Returns `Parser.EOS` if there is no more.
  getObject() {
    this._skipAtmosphere();
    let ret;
    if (this.done()) {
      ret = Parser.EOS;
    } else {
      switch (this.txt[this.i]) {
        case "#":
          this.i++;
          ret = this._parseSharp();
          break;
        case "(":
        case "[":
        case "{":
          ret = this._parseList();
          break;
        case '"':
          ret = this._parseString();
          break;
        case "|":
          ret = this._parseEnclosedSymbol();
          break;

        // Aliases
        case "'":
          this.i++;
          ret = this._parseQuote("quote");
          break;
        case "`":
          this.i++;
          ret = this._parseQuote("quasiquote");
          break;
        case ",":
          this.i++;
          if (this.txt[this.i] == "@") {
            this.i++;
            ret = this._parseQuote("unquote-splicing");
          } else {
            ret = this._parseQuote("unquote");
          }
          break;

        default:
          ret = this._parseDecimalNumberOrIdentifier();
          break;
      }
    }
    //console.log("getObject", inspect(ret))
    return ret;
  }

  _getObjectOrThrow(msg) {
    const v = this.getObject();
    if (v === Parser.EOS) throw msg;
    return v;
  }

  // Skip whitespaces
  _skipWhitespace() {
    while (this.i < this.txt.length) {
      switch (this.txt[i]) {
        case " ":
        case "\t":
        case "\n":
          i++;
          break;
        default:
          return;
      }
    }
  }

  // Skip whitespace, comment and directive
  // Note: calling this method may change parser state (`this.foldCase`).
  _skipAtmosphere() {
    while (this.i < this.txt.length) {
      switch (this.txt[this.i]) {
        // Whitespace
        case " ":
        case "\t":
        case "\n":
          this.i++;
          break;

        // Line comment
        case ";":
          const m = this.match(/^;[^\n]*(\n|$)/);
          this.i += m[0].length;
          break;

        // Sexp/multiline comment and directives
        case "#":
          if (this.txt[this.i+1] == ";") {
            this.i += "#;".length;
            this._skipAtmosphere();
            // Drop the value after the `#;` and continue
            this._getObjectOrThrow("missing argument for `#;`");
          } else if (this.txt[this.i+1] == "|") {
            this.i += "#|".length;
            this._skipBlockComment();
          } else if (this.match(/^#!fold-case/)) {
            this.i += "#!fold-case".length;
            this.foldCase = true;
          } else if (this.match(/^#!no-fold-case/)) {
            this.i += "#!no-fold-case".length;
            this.foldCase = false;
          } else {
            return;
          }
          break;

        default:
          return;
      }
    }
  }

  // Skip block comment (`#|...|#`)
  // `#|` must be consumed beforehand
  _skipBlockComment() {
    let level = 1;
    while (this.i < this.txt.length) {
      const mEnd = this.match(/\|#/);
      if (mEnd === null) {
        break;
      }
      // Check nested comment
      const mBegin = /#\|/.exec(this.txt.slice(this.i, mEnd.index));
      if (mBegin) {
        level++;
        this.i += mBegin.index + "#|".length;
        continue;
      } else {
        this.i += mEnd.index + "|#".length;
        level--;
        if (level == 0) {
          // Found matching `|#` for all `#|`s.
          return;
        }
      }
    }
    throw new BiwaError("unterminated block comment (`|#` not found)", this.rest());
  }

  // Parse quote, backquote, unquote, unquote-splicing
  _parseQuote(name) {
    this._skipAtmosphere();
    const v = this._getObjectOrThrow(`unterminated ${name}`);
    return List(Sym(name), v);
  }

  // Parse stuffs starting with `#` (except those parsed with _skipAtmosphere)
  _parseSharp() {
    switch (this.txt[this.i]) {
      case "t":
        if (this.match(/^true/)) {
          this.i += "true".length;
        } else {
          this.i++;
        }
        return true;
      case "f":
        if (this.match(/^false/)) {
          this.i += "false".length;
        } else {
          this.i++;
        }
        return false;
      case "\\":
        this.i++;
        return this._parseChar();
      case "(":
        this.i++;
        return this._parseVector();
      case "u":
        if (this.match(/^u8\(/)) {
          throw new BiwaError("bytevectors are not supported yet", this.rest(-1));
        } else {
          break;
        }
      case "e":
      case "i":
      case "b":
      case "o":
      case "d":
      case "x":
        this.i--; // Unget `#`
        return this._parsePrefixedNumber();
      default:
        if (this.match(/^\d/)) {
          return this._parseDatumLabel();
        } else {
          break;
        }
    }
    throw new BiwaError("unknown #-syntax", this.rest(-1));
  }

  // Parse a character literal after `#\`
  _parseChar() {
    let m = this.match(NAMED_CHARS_REXP);
    if (m) {
      this.i += m[0].length;
      return Char.get(NAMED_CHARS[m[1]]);
    }
    m = this.match(/^x([a-zA-Z0-9]+)/);
    if (m) {
      this.i += m[0].length;
      return Char.get(String.fromCharCode(parseInt(m[1], 16)));
    }
    if (this.done()) {
      throw new BiwaError("got EOS on char literal", this.rest(-2));
    } else {
      const c = this.txt[this.i];
      this.i++;
      return Char.get(c);
    }
  }

  // Parse a vector expression after `#(`
  _parseVector() {
    const begin = this.i;
    const arr = [];
    loop: while (this.i < this.txt.length) {
      this._skipAtmosphere();
      switch (this.txt[this.i]) {
        case ")":
          this.i++;
          break loop;
        case "]":
        case "}":
          throw new BiwaError("extra close paren", this.rest());
        default:
          arr.push(this.getObject());
          break;
      }
    }
    return arr;
  }

  // Parse a number prefixed with `#i`, `#b`, etc.
  _parsePrefixedNumber() {
    let base = 10; // Decimal is the default
    if (this.match(/^#[iIeE]/)) this.i += 2; // Exactness is not supported.
    if      (this.match(/^#[bB]/)) { base = 2; this.i += 2 }
    else if (this.match(/^#[oO]/)) { base = 8; this.i += 2 }
    else if (this.match(/^#[dD]/)) { base = 10; this.i += 2 }
    else if (this.match(/^#[xX]/)) { base = 16; this.i += 2 }
    if (this.match(/^#[iIeE]/)) this.i += 2; // Exactness is not supported.

    return this._parseComplexNumber(base);
  }

  // Parse a (possibly) complex number
  _parseComplexNumber(base) {
    const a = this._parseRealNumber(base);
    const c = this.txt[this.i];
    if (c == "@") {
      this.i++;
      return this._parsePolarComplexNumber(a, base);
    } else if (c == "+" || c == "-") {
      this.i--; // Unget the sign
      return this._parseOrthoComplexNumber(a, base);
    } else {
      // Was not a complex number.
      return a;
    }
  }

  // Parse a complex number of the form `a@b`
  _parsePolarComplexNumber(a, base) {
    const b = this._parseRealNumber(base);
    return Complex.from_polar(a, b);
  }

  // Parse a complex number of the form `a+bi`
  _parseOrthoComplexNumber(a, base) {
    const b = this._parseRealNumber(base);
    if (this.match(/^[iI]/)) {
      this.i++;
      return new Complex(a, b)
    } else {
      throw new BiwaError("invalid complex number format (missing `i`)", this.rest())
    }
  }

  // Parse a real number in base 2, 8, or 16
  // If `maybeSymbol` is true, returns a consumed string intead of throwing error
  // when it is not a number.
  _parseRealNumber(base, maybeSymbol=false) {
    if (maybeSymbol && base != 10) throw new Bug("base must be 10 if maybeSymbol");
    let asSym = "";

    // Check if it is inf or nan
    const m = this.match(/^(\+|-)(inf.0|nan.0)/);
    if (m) {
      this.i += "+inf.0".length;
      return (m[2] == "inf.0" ? Infinity : NaN) * (m[1] == "+" ? 1 : -1);
    }

    let sign = 1;
    if (this.match(/^\+/)) {      this.i++; asSym += "+"; }
    else if (this.match(/^\-/)) { this.i++; asSym += "-"; sign = -1; }

    let a = null;
    const mm = this.match(DIGITS[base]);
    if (mm) {
      this.i += mm[0].length;
      asSym += mm[0];
      a = parseInt(mm[0], base) * sign;
    } else if (base == 10 && this.txt[this.i] == ".") {
      // May be a decimal of the form `.123`. Continue parsing
    } else if (maybeSymbol) {
      // Continue parsing.
    } else {
      throw new BiwaError("invalid char in number literal", this.rest());
    }

    // Parse rational
    if (this.txt[this.i] == "/") {
      this.i++;
      const mmm = this.match(DIGITS[base]);
      if (mmm) {
        this.i += mmm[0].length;
        const b = parseInt(mmm[0], base);
        return new Rational(a, b);
      } else if (maybeSymbol) {
        asSym += "/";
      } else {
        throw new BiwaError("invalid char in rational number literal", this.rest());
      }
    }

    // Was not a rational. Check if it is a decimal
    if (base == 10) {
      // Try matching form the beginning
      this.i -= asSym.length;
      const mmm = this.match(/^[+-]?(\d+\.\d*|\.?\d+)([eE][+-]?\d+)?/)
      if (mmm) {
        this.i += mmm[0].length;
        return parseFloat(mmm[0]);
      } else {
        // Was not a decimal either. Put back the cursor
        this.i += asSym.length;
      }
    }

    if (maybeSymbol) {
      return asSym;
    } else {
      throw new BiwaError(`invalid chars in number literal (${asSym})`, this.rest());
    }
  }

  // Parse a datum label definition (`#0=`) or reference (`#0#`).
  _parseDatumLabel() {
    const m = this.match(/^(\d+)(=|#)/);
    if (m) {
      this.i += m[0].length;
      const id = parseInt(m[1]);
      if (m[2] == "=") {
        const v = this._getObjectOrThrow("got EOS for labelled datum");
        this.labelledData[id] = v;
        return v;
      } else {
        if (this.labelledData.hasOwnProperty(id)) {
          return this.labelledData[id];
        } else {
          throw new BiwaError("undefined datum reference", this.rest(-1));
        }
      }
    } else {
      throw new BiwaError("unknown #-syntax", this.rest(-1));
    }
  }

  // Parse a list (`(...)`)
  // BiwaScheme allows `[]`, `{}` for list too 
  _parseList() {
    const begin = this.i;
    const openParen = this.txt[this.i];
    this.i++;
    const closeParen = PAREN[openParen];
    let list = nil, prev = list;
    while (this.i < this.txt.length) {
      this._skipAtmosphere();
      const c = this.txt[this.i];
      if (c == closeParen) {
        this.i++;
        break;
      } else if (c == ")" || c == "]" || c == "}") {
        throw new BiwaError("extra close paren", this.rest());
      } else if (c == "." && this.match(/^\.[\s]/)) {
        if (list === nil) {
          throw new BiwaError("no list element before `.`", this.from(begin));
        }
        this.i++;
        const v = this.getObject();
        if (v === Parser.EOS) {
          throw new BiwaError("found EOS after `.` in list", this.from(begin));
        }
        prev.cdr = v;
      } else {
        const vv = this.getObject();
        if (vv === Parser.EOS) {
          this.i = begin;
          throw new BiwaError("found EOS in list", this.rest());
        }
        const cur = new Pair(vv, nil);
        if (list === nil) list = cur;
        else prev.cdr = cur;
        prev = cur;
      }
    }
    return list;
  }

  // Parse a string literal (`"..."`)
  _parseString() {
    const m = this.match(/^"((\\"|[^"])*)"/);
    if (m) {
      this.i += m[0].length;
      const s = m[1].replaceAll(/\\\s*\n\s*/g, "");
      return this._replaceEscapedChars(s);
    } else {
      throw new BiwaError("invalid string literal", this.rest())
    }
  }

  // Parse a symbol enclosed with `|`
  _parseEnclosedSymbol() {
    const m = this.match(/^\|((\\\||[^\|])*)\|/);
    if (m) {
      this.i += m[0].length;
      return Sym(this._replaceEscapedChars(m[1]));
    } else {
      throw new BiwaError("invalid symbol literal", this.rest())
    }
  }

  // Replace `\n`, `\t`, `\x1234`, etc. in `s`
  _replaceEscapedChars(s) {
    return s
      .replaceAll(/\\x([0-9a-f]+);/ig, (_, n) => String.fromCharCode(parseInt(n, 16)))
      .replaceAll(/\\(.)/g, (_, c) => ESCAPE_SEQUENCES[c] || c);
  }

  // Parse a number or an identifier.
  _parseDecimalNumberOrIdentifier() {
    const c = this.txt[this.i];
    if (c == "#") throw new Bug("#-syntax must be parsed beforehand")
    if (c === undefined) throw new Bug("EOS must be handled beforehand")

    let v = this._parseRealNumber(10, true);
    if (isString(v)) {
      // Read the rest of this identifier
      const m = this.match(/^[^\s)}\]]+/);
      if (m) {
        this.i += m[0].length;
        v += m[0];
      }
      if (this.foldCase) {
        v = v.toLowerCase();
      }
      return Sym(v);
    } else {
      return v;
    }
  }

  // Returns rest of the source text
  rest(di = 0) {
    return this.txt.slice(this.i + di);
  }

  // Returns the source text beginning from `pos`
  from(pos) {
    return this.txt.slice(pos);
  }

  // Execute regexp match from the current position.
  // Returns `null` if no match
  match(rexp, di = 0) {
    return rexp.exec(this.rest(di));
  }

  // Returns if whole text is seen
  done() {
    return this.i >= this.txt.length;
  }
} 

// Indicates end of source file
Parser.EOS = new Object();
Parser.EOS.toString = () => "#<BiwaScheme.Parser.EOS>";

class Parser0 {
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
    if (r == Parser0.EOS)
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
        if( o != Parser0.EOS && list != nil ) {
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
      if ((this.getObject() == Parser0.EOS) || (this.i >= this.tokens.length))
        throw new BiwaError(err_msg);  
    }
  } 

  // Read a Scheme object
  // Returns `sexpCommentMarker` if `#;` is found
  getObject0() {
    if( this.i >= this.tokens.length )
      return Parser0.EOS;

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
        return t.replace(/(\r?\n|\\n)/g, "\n").replace( /^\"|\\(.|$)|\"$/g, function($0,$1) {
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
Parser0.EOS = new Object();

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
