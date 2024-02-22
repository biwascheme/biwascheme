import { nil } from "../../src/header.js"
import { Pair } from "../../src/system/pair.js"
import Parser from "../../src/system/parser.js"

function parse(txt) {
  return new Parser(txt).getObject();
}

describe("Parser (errneous)", () => {
  test("Unterminated list", () => {
    expect(() => parse("(1 2")).toThrow(Parser.Unterminated);
  })

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
})
