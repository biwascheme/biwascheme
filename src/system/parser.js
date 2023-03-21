import { nil } from "../header.js"
import { inspect } from "./_writer.js"
import { Char } from "./char.js"
import { BiwaError, Bug } from "./error.js"
import { Pair, List } from "./pair.js"
import { Sym } from "./symbol.js"
import { isSymbol, isString } from "./_types.js"
import { Complex } from "./number.js"

// Raised when input is not terminated
class Unterminated extends BiwaError {
}

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

  _getObjectOrThrowUnterminated(msg) {
    const v = this.getObject();
    if (v === Parser.EOS) throw new Unterminated(msg);
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
            this._getObjectOrThrowUnterminated("missing argument for `#;`");
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
    throw new Unterminated("unterminated block comment (`|#` not found)", this.rest());
  }

  // Parse quote, backquote, unquote, unquote-splicing
  _parseQuote(name) {
    this._skipAtmosphere();
    const v = this._getObjectOrThrowUnterminated(`unterminated ${name}`);
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
      throw new Unterminated("got EOS on char literal", this.rest(-2));
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
        const v = this._getObjectOrThrowUnterminated("got EOS for labelled datum");
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
        return list;
      } else if (c == ")" || c == "]" || c == "}") {
        throw new BiwaError("extra close paren", this.rest());
      } else if (c == "." && this.match(/^\.[\s]/)) {
        if (list === nil) {
          throw new BiwaError("no list element before `.`", this.from(begin));
        }
        this.i++;
        const v = this.getObject();
        if (v === Parser.EOS) {
          throw new Unterminated("found EOS after `.` in list", this.from(begin));
        }
        prev.cdr = v;
      } else {
        const vv = this.getObject();
        if (vv === Parser.EOS) {
          this.i = begin;
          throw new Unterminated("found EOS in list", this.rest());
        }
        const cur = new Pair(vv, nil);
        if (list === nil) list = cur;
        else prev.cdr = cur;
        prev = cur;
      }
    }
    this.i = begin;
    throw new Unterminated("found EOS in list", this.rest());
  }

  // Parse a string literal (`"..."`)
  _parseString() {
    const m = this.match(/^"((\\"|[^"])*)"/);
    if (m) {
      this.i += m[0].length;
      const s = m[1].replaceAll(/\\\s*\n\s*/g, "");
      return this._replaceEscapedChars(s);
    } else {
      throw new Unterminated("invalid string literal", this.rest())
    }
  }

  // Parse a symbol enclosed with `|`
  _parseEnclosedSymbol() {
    const m = this.match(/^\|((\\\||[^\|])*)\|/);
    if (m) {
      this.i += m[0].length;
      return Sym(this._replaceEscapedChars(m[1]));
    } else {
      throw new Unterminated("invalid symbol literal", this.rest())
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

Parser.Unterminated = Unterminated;
  
export default Parser;
