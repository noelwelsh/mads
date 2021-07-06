package mads

import munit.FunSuite

class ParserSuite extends FunSuite {
  import Parser.Result

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
      Result.Success("123", "123abc", 0, 3)
    )
    assertEquals(
      Parser.charsWhile(_.isDigit).parse("123"),
      Result.Continue("123", "123", 0)
    )
    assertEquals(
      Parser.charsWhile(_.isDigit).parse("abc"),
      Result.Success("", "abc", 0, 0)
    )
  }

  test("Parser.charsUntilTerminator") {
    assertEquals(
      Parser.charsUntilTerminator("abc").parse("123abc"),
      Result.Success("123", "123abc", 0, 6)
    )
    assertEquals(
      Parser.charsUntilTerminator("a", "bc").parse("123abc"),
      Result.Success("123", "123abc", 0, 4)
    )
    assertEquals(
      Parser.charsUntilTerminator("a", "b").parse("123b"),
      Result.Success("123", "123b", 0, 4)
    )
    assertEquals(
      Parser.charsUntilTerminator("abc").parse("123"),
      Result.Committed("123", 0, 3)
    )
  }

  test("Parser.charsUntilTerminatorOrEnd") {
    assertEquals(
      Parser.charsUntilTerminatorOrEnd("\n").parse("123abc"),
      Result.Continue("123abc", "123abc", 0)
    )
    assertEquals(
      Parser.charsUntilTerminatorOrEnd("\n").parse("123abc\n"),
      Result.Success("123abc", "123abc\n", 0, 7)
    )
    assertEquals(
      Parser.charsUntilTerminatorOrEnd(">", "<").parse("123<"),
      Result.Success("123", "123<", 0, 4)
    )
    assertEquals(
      Parser.charsUntilTerminatorOrEnd(">", "<").parse("123>"),
      Result.Success("123", "123>", 0, 4)
    )
    assertEquals(
      Parser.charsUntilTerminatorOrEnd(">", "<").parse("123"),
      Result.Continue("123", "123", 0)
    )
  }

  test("Parser.stringIn") {
    val p = Parser.stringIn(List("#", "##", "###"))
    assertEquals(p.parse("#"), Result.Success("#", "#", 0, 1))
    assertEquals(p.parse("##"), Result.Success("##", "##", 0, 2))
    assertEquals(p.parse("###"), Result.Success("###", "###", 0, 3))
    assertEquals(p.parse("####"), Result.Success("###", "####", 0, 3))
    assert(p.parse("abc").isFailure)
  }

  test("Parser.~") {
    val whiteSpace = Parser.charsWhile(ch => ch == ' ' || ch == '\t')
    val lineEnd = Parser.string("\n")
    val p = whiteSpace ~ lineEnd

    val ws = "    "
    val lf = "\n"
    val input = ws ++ lf

    assertEquals(whiteSpace.parse(ws), Result.Continue(ws, ws, 0))
    assertEquals(whiteSpace.parse(input), Result.Success(ws, input, 0, ws.size))
    assertEquals(lineEnd.parse(lf), Result.Success(lf, lf, 0, lf.size))
    assertEquals(p.parse(input), Result.Success((ws, lf), input, 0, input.size))
  }

  test("Parser.end") {
    val end = Parser.end
    val whiteSpace = Parser.charsWhile(ch => ch == ' ' || ch == '\t')
    val parser = whiteSpace <* end
    val input = "    "

    assertEquals(parser.parse(input), Result.Success(input, input, 0, input.size))
    assertEquals(end.parse(input), Result.Epsilon(input, 0))
  }
}
