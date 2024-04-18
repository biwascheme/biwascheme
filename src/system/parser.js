import { nil } from "../header.js"
import { inspect, to_write } from "./_writer.js"
import { Char } from "./char.js"
import { BiwaError, Bug } from "./error.js"
import { Pair, List } from "./pair.js"
import { Sym } from "./symbol.js"
import { isSymbol, isString } from "./_types.js"
import { Complex, Rational } from "./number.js"

// Raised when input is not terminated
class Unterminated extends BiwaError {}
// Raised when input contains extraneous `)`, etc.
class Unbalanced extends BiwaError {}
// Raised for invalid char literal, etc.
class Invalid extends BiwaError {}

// Matches first word-like stuff i.e. characters not space and
// not symbols that are not allowed for an identifier.
// TODO: is there other non-id symbols in Unicode?
const ID_REXP = /^[^\s\t\n\(\)\[\]\{\}\"\'\`\,\;\|]+/

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

  // Read a Scheme value and returns it.
  // Returns `Parser.EOS` if there is no more.
  // Throws `Parser.Unterminated` if the source text ends with an unterminated
  // list, etc.
  getObject() {
    this._skipAtmosphere();
    let ret;
    if (this.done()) {
      ret = Parser.EOS;
    } else {
      const tok = this._getToken();
      switch (tok) {
        case "(":
        case "[":
        case "{":
          this.i--; // Unget the first paren
          ret = this._parseList();
          break;
        case '"':
          ret = this._parseString();
          break;
        case "|":
          ret = this._parseEnclosedSymbol();
          break;
        
        case ")":
        case "]":
        case "}":
          throw new Unbalanced(`found extraneous '${tok}'`);
          break;

        // Aliases
        case "'":
          ret = this._parseQuote("quote");
          break;
        case "`":
          ret = this._parseQuote("quasiquote");
          break;
        case ",":
          ret = this._parseQuote("unquote");
          break;
        case ",@":
          ret = this._parseQuote("unquote-splicing");
          break;

        default:
          if (tok[0] == "#") {
            ret = this._parseSharp(tok);
          } else {
            ret = this._parseDecimalNumberOrIdentifier(tok);
          }
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

  // Returns current token and proceeds the cursor.
  _getToken() {
    const c = this.txt[this.i];
    if (c === undefined) throw new Bug("EOS must be handled beforehand")
    let tok;
    switch (c) {
      case "(":
      case "[":
      case "{":
      case ")":
      case "]":
      case "}":
      case "'":
      case "`":
      case '"':
      case "|":
        this.i++;
        tok = c;
        break;
      case ",":
        this.i++;
        if (this.txt[this.i] == "@") {
          this.i++;
          tok = ",@";
        } else {
          tok = ",";
        }
        break;
      case "#":
        this.i++;
        const c2 = this.txt[this.i];
        switch (c2) {
          case "(":
            this.i++;
            tok = "#(";
            break;
          case "\\":
            this.i++;
            tok = "#\\" + this._getCharToken();
            break;
          default:
            const m = this.match(ID_REXP);
            if (m) {
              this.i += m[0].length;
              tok = "#" + m[0];
            } else if (c2 === undefined) {
              // Program ends with an `#` (invalid)
              tok = "#";
            } else {
              // Invalid symbol after `#`
              this.i++;
              tok = "#" + c2;
            }
            break;
        }
        break;
      default:
        const m = this.match(ID_REXP);
        if (m) {
          this.i += m[0].length;
          tok = m[0];
        } else {
          this.i++;
          tok = c;
        }
        break;
    }
    return tok;
  }

  // Get the possible part after `#\`
  _getCharToken() {
    if (this.done()) {
      throw new Unterminated("got EOS on char literal", this.rest(-2));
    }
    const m = this.match(/^(\w+)/);
    if (m) {
      return m[0];
    } else {
      const c = this.txt[this.i];
      this.i++;
      return c;
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
  _parseSharp(tok) {
    if (!tok.startsWith("#")) throw new Bug("not a sharp");
    switch (tok) {
      case "#t":
      case "#true":
        return true;
      case "#f":
      case "#false":
        return false;
      case "#(":
        return this._parseVector();
      default:
        switch (true) {
          case tok.startsWith("#\\"):
            return this._parseChar(tok);
          case tok.startsWith("#u8"):
            throw new BiwaError("bytevectors are not supported yet", this.rest(-1));
          case tok.match(/^#[eibodx]/):
            return this._parsePrefixedNumber(tok);
          case tok.match(/^#\d/):
            return this._parseDatumLabel(tok);
          default:
            throw new Invalid("unknown #-syntax", tok);
        }
    }
  }

  // Parse a character literal (begins with `#\`)
  _parseChar(tok) {
    if (!tok.startsWith("#\\")) throw new Bug("not a char");
    const body = tok.substring(2);
    // Named char?
    let c = NAMED_CHARS[body];
    if (c) {
      return Char.get(c);
    }
    // Specified by code?
    let m = /^x([a-zA-Z0-9]+)$/.exec(body);
    if (m) {
      return Char.get(String.fromCharCode(parseInt(m[1], 16)));
    }
    // A character?
    if (body.length == 1) {
      return Char.get(body);
    }
    // Otherwise
    throw new Invalid(`Invalid char literal: ${tok}`);
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
  // Throws error if not a prefixed number.
  _parsePrefixedNumber(tok) {
    let base = 10; // Decimal is the default
    let s = tok;
    if (s.match(/^#[iIeE]/)) s = s.substring(2); // Exactness is not supported.
    if      (s.match(/^#[bB]/)) { base = 2;  s = s.substring(2); }
    else if (s.match(/^#[oO]/)) { base = 8;  s = s.substring(2); }
    else if (s.match(/^#[dD]/)) { base = 10; s = s.substring(2); }
    else if (s.match(/^#[xX]/)) { base = 16; s = s.substring(2); }
    if (s.match(/^#[iIeE]/)) s = s.substring(2); // Exactness is not supported.

    const v = this._parseComplexNumber(base, s);
    if (v === null) {
      // Extraneous char after number prefix (like `#dfoo`)
      throw new Invalid(`invalid number format: %{tok}`);
    } else {
      return v;
    }
  }

  // Parse a complex (or real) number.
  // Returns null if not a number.
  _parseComplexNumber(base, tok) {
    const [a, rest] = this._parseRealNumber(base, tok);
    if (a === null) {
      return null;
    } else if (rest === "") {
      return a;
    }
    const c = rest[0];
    if (c == "@") {
      // Polar complex number? (eg. `1@2`)
      const [b, rest2] = this._parseRealNumber(base, rest.substring(1));
      if (b !== null && rest2 === "") {
        return Complex.from_polar(a, b);
      }
    } else if (c == "+" || c == "-") {
      // Ortho complex number? (eg. `1+2i`)
      const [b, rest2] = this._parseRealNumber(base, rest);
      if (b !== null && rest2.match(/^[iI]$/)) {
        return new Complex(a, b);
      }
    }
    return null;
  }

  // Parse a real (or rational) number at the head of `tok`.
  // Returns the number and the rest of the string.
  // Returns [null, tok] if not a number.
  _parseRealNumber(base, tok) {
    // inf or nan
    const m = tok.match(/^(\+|-)(inf.0|nan.0)/);
    if (m) {
      const v = (m[2] == "inf.0" ? Infinity : NaN) * (m[1] == "+" ? 1 : -1);
      return [v, tok.substring(m[0].length)];
    }

    // Handle the sign in case this is a number
    let s = tok;
    let sign = 1;
    if (s.match(/^\+/)) {      s = s.substring(1); }
    else if (s.match(/^\-/)) { s = s.substring(1); sign = -1; }

    // Handle deciaml of the form `.123`
    if (base == 10 && s[0] == ".") {
      const mm = tok.match(/^\.(\d+)([eE][+-]?\d+)?$/)
      if (mm) {
        return [parseFloat("0"+mm[0]), tok.substring(mm[0].length)];
      } else {
        return [null, tok];
      }
    }

    // Get the first number part
    let a = 0;
    const mm = s.match(DIGITS[base]);
    if (mm) {
      s = s.substring(mm[0].length);
      a = parseInt(mm[0], base) * sign;
    } else {
      return [null, tok];
    }
    // Parse rational
    if (s[0] == "/") {
      s = s.substring(1);
      const mmm = s.match(DIGITS[base]);
      if (mmm) {
        s = s.substring(mmm[0]);
        const b = parseInt(mmm[0], base);
        return [new Rational(a, b), s];
      } else {
        return [null, tok];
      }
    }

    // Was not a rational. Check if it is a decimal
    if (base == 10) {
      // Try matching form the beginning
      const mmm = tok.match(/^[+-]?(\d+\.\d*|\.?\d+)([eE][+-]?\d+)?/)
      if (mmm) {
        return [parseFloat(mmm[0]), tok.substring(mmm[0].length)];
      }
    }

    return [null, tok];
  }

  // Parse a datum label definition (`#0=`) or reference (`#0#`).
  _parseDatumLabel(tok) {
    const m = tok.match(/^#(\d+)(=|#)$/);
    if (m) {
      const id = parseInt(m[1]);
      if (m[2] == "=") {
        // Definition
        const v = this._getObjectOrThrowUnterminated("got EOS for labelled datum");
        this.labelledData[id] = v;
        return v;
      } else {
        // Reference
        if (this.labelledData.hasOwnProperty(id)) {
          return this.labelledData[id];
        } else {
          throw new BiwaError("undefined datum reference", tok);
        }
      }
    } else {
      throw new BiwaError("unknown #-syntax", tok);
    }
  }

  // Parse a list (`(...)`)
  // BiwaScheme allows `[]`, `{}` for list too 
  _parseList() {
    const begin = this.i;
    const openParen = this.txt[this.i];
    this.i++;
    const closeParen = PAREN[openParen];
    let dotSeen = false;
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
        dotSeen = true;
        this.i++;
        const v = this.getObject();
        if (v === Parser.EOS) {
          throw new Unterminated("found EOS after `.` in list", this.from(begin));
        }
        prev.cdr = v;
      } else if (dotSeen) {
          throw new BiwaError("more than one element after `.`", this.from(begin));
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

  // Parse a string literal after the first `"`
  _parseString() {
    const m = this.match(/^((\\"|[^"])*)"/);
    if (m) {
      this.i += m[0].length;
      const s = m[1].replaceAll(/\\\s*\n\s*/g, "");
      return this._replaceEscapedChars(s);
    } else {
      throw new Unterminated("invalid string literal", this.rest())
    }
  }

  // Parse a symbol enclosed with `|` after the first `|`
  _parseEnclosedSymbol() {
    const m = this.match(/^((\\\||[^\|])*)\|/);
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
  _parseDecimalNumberOrIdentifier(tok) {
    if (tok === "") throw new Bug("empty string given")

    const v = this._parseComplexNumber(10, tok);
    if (v === null) {
      return Sym(this.foldCase ? tok.toLowerCase() : tok);
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
