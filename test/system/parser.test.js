import { nil } from "../../src/header.js"
import { Pair, List } from "../../src/system/pair.js"
import { Sym } from "../../src/system/symbol.js"
import { Char } from "../../src/system/char.js"
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

  test("nil", () => {
    expect(parse("()")).toBe(nil);
  })

  describe("numbers", () => {
    test("integer", () => {
      expect(parse("1")).toBe(1);
    })
  })

  describe("symbols", () => {
    test("simple", () => {
      expect(parse("foo")).toBe(Sym("foo"));
    })

    test("starts with number", () => {
      expect(parse("1+")).toBe(Sym("1+"));
    })

    test("whitespace", () => {
      expect(parse('| |')).toBe(Sym(' '));
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
  })

  describe("chars", () => {
    test("simple", () => {
      expect(parse("#\\a")).toBe(Char.get("a"));
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

    test("unknown name", () => {
      expect(() => parse("#\\foo")).toThrow(Parser.Invalid)
    })
  })

  describe("lists", () => {
    test("number list", () => {
      expect(parse("(1 2)")).toBe(List(1, 2));
    })

    test("Unterminated list", () => {
      expect(() => parse("(1 2")).toThrow(Parser.Unterminated);
    })
  })
})
