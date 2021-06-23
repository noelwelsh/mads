package mads

import munit.FunSuite

class ParserSuite extends FunSuite {
  test("Parser.char") {
    assertEquals(Parser.char('a').parseOrExn("a"), 'a')
    assert(Parser.char('a').parse("b").isFailure)
  }

  test("Parser.charWhere") {
    assertEquals(Parser.charWhere(_.isDigit).parseOrExn("1"), '1')
    assert(Parser.charWhere(_.isDigit).parse("b").isFailure)
  }

  test("Parser.charsWhile") {
    assertEquals(Parser.charsWhile(_.isDigit).parseOrExn("123abc"), "123")
    assert(Parser.charsWhile(_.isDigit).parse("b").isFailure)
  }
}
