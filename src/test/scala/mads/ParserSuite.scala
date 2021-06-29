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
    assertEquals(
      Parser.charsWhile(_.isDigit).parse("123abc"),
      Complete.Success("123", "123abc", 3)
    )
    assertEquals(
      Parser.charsWhile(_.isDigit).parse("123"),
      Complete.Success("123", "123", 3)
    )
    assertEquals(
      Parser.charsWhile(_.isDigit).parse("abc"),
      Complete.Success("", "abc", 0)
    )
  }

  test("Parser.charsUntilTerminator") {
    assertEquals(
      Parser.charsUntilTerminator("abc").parse("123abc"),
      Complete.Success("123", "123abc", 6)
    )
    assertEquals(
      Parser.charsUntilTerminator("a", "bc").parse("123abc"),
      Complete.Success("123", "123abc", 4)
    )
    assertEquals(
      Parser.charsUntilTerminator("a", "b").parse("123b"),
      Complete.Success("123", "123b", 4)
    )
    assertEquals(
      Parser.charsUntilTerminator("abc").parse("123"),
      Complete.Committed("123", 3)
    )
  }

  test("Parser.charsUntilTerminatorOrEnd") {
    assertEquals(
      Parser.charsUntilTerminatorOrEnd("\n").parse("123abc"),
      Complete.Success("123abc", "123abc", 6)
    )
    assertEquals(
      Parser.charsUntilTerminatorOrEnd("\n").parse("123abc\n"),
      Complete.Success("123abc", "123abc\n", 7)
    )
    assertEquals(
      Parser.charsUntilTerminatorOrEnd(">", "<").parse("123<"),
      Complete.Success("123", "123<", 4)
    )
    assertEquals(
      Parser.charsUntilTerminatorOrEnd(">", "<").parse("123>"),
      Complete.Success("123", "123>", 4)
    )
    assertEquals(
      Parser.charsUntilTerminatorOrEnd(">", "<").parse("123"),
      Complete.Success("123", "123", 3)
    )
  }

  test("Parser.stringIn") {
    val p = Parser.stringIn(List("#", "##", "###"))
    assertEquals(p.parse("#"), Complete.Success("#", "#", 1))
    assertEquals(p.parse("##"), Complete.Success("##", "##", 2))
    assertEquals(p.parse("###"), Complete.Success("###", "###", 3))
    assertEquals(p.parse("####"), Complete.Success("###", "####", 3))
    assert(p.parse("abc").isFailure)
  }
}
