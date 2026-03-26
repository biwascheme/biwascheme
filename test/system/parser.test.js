import { nil } from "../../src/header.js"
import { Pair, List } from "../../src/system/pair.js"
import { Sym } from "../../src/system/symbol.js"
import { Char } from "../../src/system/char.js"
import { Complex, Rational } from "../../src/system/number.js"
import { to_write } from "../../src/system/_writer.js"
import Parser from "../../src/system/parser.js"

function parse(txt) {
  return new Parser(txt).getObject();
}

describe("Parser (errneous)", () => {
  test("Extra closing paren", () => {
    const p = new Parser("(display 10))");
    expect(p.getObject()).toBeInstanceOf(Pair);
    expect(() => p.getObject()).toThrow(Parser.Unbalanced)
  })
})

describe("Parser", () => {
  test("EOS", () => {
    expect(parse("")).toBe(Parser.EOS);
  })

  describe("atmosphere", () => {
    test("tab", () => {
      expect(parse("#(1\t2)")).toEqual([1, 2]);
    })

    test("line comment", () => {
      expect(parse("#(1; comment\n 2)")).toEqual([1, 2]);
    })

    test("block comment", () => {
      expect(parse("#(1 #| comment |# 2)")).toEqual([1, 2]);
    })

    test("nested block comment", () => {
      expect(parse("#(#| #| inner |# outer |# 1)")).toEqual([1]);
    })

    test("unterminated block comment", () => {
      expect(() => parse("#| oops")).toThrow(Parser.Unterminated);
    })

    test("sexp comment", () => {
      expect(parse("#(1 #;() 2)")).toEqual([1, 2]);
    })

    test("multiple sexp comments", () => {
      expect(parse("#(#;1 #;2 3)")).toEqual([3]);
    })
  })

  describe("case directive", () => {
    test("fold to no-fold", () => {
      expect(parse("#( #!fold-case A #!no-fold-case B)")).toEqual([Sym("a"), Sym("B")]);
    })

    test("no-fold to fold", () => {
      expect(parse("#( #!no-fold-case A #!fold-case B)")).toEqual([Sym("A"), Sym("b")]);
    })
  })

  test("nil", () => {
    expect(parse("()")).toBe(nil);
  })

  test("boolean", () => {
    expect(parse("#(#t #true #f #false)")).toEqual([true, true, false, false]);
  })

  describe("numbers", () => {
    test("sharp", () => {
      expect(parse("#b100")).toBe(4);
      expect(parse("#B100")).toBe(4);
      expect(parse("#d100")).toBe(100);
      expect(parse("#D100")).toBe(100);
      expect(parse("#o77")).toBe(63);
      expect(parse("#O77")).toBe(63);
      expect(parse("#xFF")).toBe(255);
      expect(parse("#XFF")).toBe(255);

      expect(parse("#ib100")).toBe(4);
      expect(parse("#EB100")).toBe(4);
      expect(parse("#bI100")).toBe(4);
      expect(parse("#Be100")).toBe(4);
    })

    test("negative with base prefix", () => {
      expect(parse("#b-100")).toBe(-4);
      expect(parse("#x-FF")).toBe(-255);
    })

    test("exactness prefix is silently stripped", () => {
      expect(parse("#e1")).toBe(1);
      expect(parse("#i1")).toBe(1);
    })

    test("invalid number after prefix", () => {
      expect(() => parse("#dfoo")).toThrow();
    })

    test("integer", () => {
      expect(parse("1")).toBe(1);
    })

    test("negative", () => {
      expect(parse("-1")).toBe(-1);
      expect(parse("-1.5")).toBe(-1.5);
      expect(parse("-1/2")).toEqual(new Rational(-1, 2));
    })

    test("positive sign", () => {
      expect(parse("+1")).toBe(1);
    })

    test("decimal", () => {
      expect(parse("1.2")).toBe(1.2);
      expect(parse(".2")).toBe(0.2);
    })

    test("exponent", () => {
      expect(parse("1e10")).toBe(1e10);
      expect(parse("1.5e-3")).toBe(1.5e-3);
    })

    test("special floats", () => {
      expect(parse("+inf.0")).toBe(Infinity);
      expect(parse("-inf.0")).toBe(-Infinity);
      expect(parse("+nan.0")).toBeNaN();
    })

    test("rational", () => {
      expect(parse("1/2")).toEqual(new Rational(1, 2));
    })

    test("polar complex", () => {
      expect(parse("1@2")).toEqual(Complex.from_polar(1, 2));
    })

    test("ortho complex", () => {
      expect(parse("-1+2i")).toEqual(new Complex(-1, 2));
    })

    test("rational in complex", () => {
      expect(parse("1/2+3/4i")).toEqual(new Complex(new Rational(1, 2), new Rational(3, 4)));
    })

    test("decimal in complex", () => {
      expect(parse(".2+2.3i")).toEqual(new Complex(0.2, 2.3));
    })
  })

  describe("symbols", () => {
    test("simple", () => {
      expect(parse("foo")).toBe(Sym("foo"));
    })

    test("starts with integer", () => {
      expect(to_write(parse("(1+ 2)"))).toBe("(1+ 2)");
    })

    test("starts with decimal", () => {
      expect(to_write(parse("(.1+ 2)"))).toBe("(.1+ 2)");
    })

    test("starts with rational", () => {
      expect(to_write(parse("(1/2+ 3)"))).toBe("(1/2+ 3)");
    })

    test("whitespace", () => {
      expect(parse('| |')).toBe(Sym(' '));
    })

    test("newline", () => {
      expect(parse('|a\nb|')).toBe(Sym('a\nb'));
    })
  })

  describe("strings", () => {
    test("simple", () => {
      expect(parse('"hello"')).toBe("hello");
    })

    test("whitespace", () => {
      expect(parse('"hello world"')).toBe("hello world");
    })

    test("tab", () => {
      expect(parse('"hello\tworld"')).toBe("hello\tworld");
    })

    test("newline", () => {
      expect(parse('"hello\nworld"')).toBe("hello\nworld");
    })

    test("escape sequences", () => {
      expect(parse('"\\n"')).toBe("\n");
      expect(parse('"\\t"')).toBe("\t");
      expect(parse('"\\a"')).toBe("\x07");
      expect(parse('"\\r"')).toBe("\x0D");
    })

    test("hex escape", () => {
      expect(parse('"\\x41;"')).toBe("A");
    })

    test("escaped double quote", () => {
      expect(parse('"say \\"hi\\""')).toBe('say "hi"');
    })

    test("line continuation", () => {
      expect(parse('"hello\\\n  world"')).toBe("helloworld");
    })

    test("unterminated", () => {
      expect(() => parse('"hello')).toThrow(Parser.Unterminated);
    })
  })

  describe("chars", () => {
    test("simple", () => {
      expect(parse("#\\a")).toBe(Char.get("a"));
    })

    test("multibyte", () => {
      expect(parse("#\\あ")).toBe(Char.get("あ"));
    })

    test("number", () => {
      expect(parse("#\\1")).toBe(Char.get("1"));
    })

    test("paren", () => {
      expect(parse("#\\(")).toBe(Char.get("("));
    })

    test("backslash", () => {
      expect(parse("#\\\\")).toBe(Char.get("\\"));
    })

    test("whitespace", () => {
      expect(parse("#\\ ")).toBe(Char.get(" "));
    })

    test("tab", () => {
      expect(parse("#\\\t")).toBe(Char.get("\t"));
    })

    test("newline", () => {
      expect(parse("#\\\n")).toBe(Char.get("\n"));
    })

    test("named", () => {
      expect(parse("#\\newline")).toBe(Char.get("\n"));
    })

    test("named: alarm/backspace/null/space/return", () => {
      expect(parse("#\\alarm")).toBe(Char.get("\x07"));
      expect(parse("#\\backspace")).toBe(Char.get("\x08"));
      expect(parse("#\\null")).toBe(Char.get("\x00"));
      expect(parse("#\\space")).toBe(Char.get(" "));
      expect(parse("#\\return")).toBe(Char.get("\x0D"));
    })

    test("hex char", () => {
      expect(parse("#\\x41")).toBe(Char.get("A"));
    })

    test("unknown name", () => {
      expect(() => parse("#\\foo")).toThrow(Parser.Invalid)
    })

    test("unterminated", () => {
      expect(() => parse("#\\")).toThrow(Parser.Unterminated);
    })
  })

  describe("lists", () => {
    test("number list", () => {
      expect(to_write(parse("(1 2)"))).toBe("(1 2)");
    })

    test("dotted pair", () => {
      expect(to_write(parse("(1 . 2)"))).toBe("(1 . 2)");
    })

    test("bracket alternative", () => {
      expect(to_write(parse("[1 2]"))).toBe("(1 2)");
    })

    test("dot before first element", () => {
      expect(() => parse("(. 2)")).toThrow();
    })

    test("multiple elements after dot", () => {
      expect(() => parse("(1 . 2 3)")).toThrow();
    })

    test("mismatched parens", () => {
      expect(() => parse("(1 2]")).toThrow();
    })

    test("Unterminated list", () => {
      expect(() => parse("(1 2")).toThrow(Parser.Unterminated);
    })
  })

  test("vector", () => {
    expect(parse("#(1 2)")).toEqual([1, 2]);
  })

  describe("quotes", () => {
    test("quoted symbol", () => {
      expect(to_write(parse("'foo"))).toBe("(quote foo)");
    })

    test("quoted number", () => {
      expect(to_write(parse("'1"))).toBe("(quote 1)");
    })

    test("double quoted", () => {
      expect(to_write(parse("''foo"))).toBe("(quote (quote foo))");
    })

    test("unterminated quote", () => {
      expect(() => parse("''")).toThrow(Parser.Unterminated);
    })

    test("quasiquote", () => {
      expect(to_write(parse("`(,foo ,@bar)"))).toBe("(quasiquote ((unquote foo) (unquote-splicing bar)))");
    })
  })

  describe("datum label", () => {
    test("basic", () => {
      expect(parse("#(#0=123 #0#)")).toEqual([123, 123]);
    })

    test("multi-digit label", () => {
      expect(parse("#(#10=foo #10#)")).toEqual([Sym("foo"), Sym("foo")]);
    })

    test("undefined reference", () => {
      expect(() => parse("#0#")).toThrow();
    })
  })

  describe("quotes", () => {
    test("unquote", () => {
      expect(to_write(parse(",foo"))).toBe("(unquote foo)");
    })

    test("unquote-splicing", () => {
      expect(to_write(parse(",@foo"))).toBe("(unquote-splicing foo)");
    })
  })

  describe("enclosed symbol", () => {
    test("escaped pipe", () => {
      expect(parse("|a\\|b|")).toBe(Sym("a|b"));
    })

    test("unterminated", () => {
      expect(() => parse("|foo")).toThrow(Parser.Unterminated);
    })
  })

  describe("Parser.parse", () => {
    test("multiple expressions", () => {
      expect(Parser.parse("1 2 3")).toEqual([1, 2, 3]);
    })

    test("empty input", () => {
      expect(Parser.parse("")).toEqual([]);
    })
  })
})
