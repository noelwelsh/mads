package mads

import munit.FunSuite

class Parser0Suite extends FunSuite {
  import Parser.Result

  test("Parser0.charsWhile") {
    assertEquals(
      Parser0.charsWhile(_.isDigit).parse("123abc"),
      Result.Success("123", "123abc", 0, 3)
    )
    assertEquals(
      Parser0.charsWhile(_.isDigit).parse("123"),
      Result.Continue("123", "123", 0)
    )
    assertEquals(
      Parser0.charsWhile(_.isDigit).parse("abc"),
      Result.Success("", "abc", 0, 0)
    )
  }

  test("Parser0.charsUntilRegexOrEnd") {
    val parser = Parser0.charsUntilRegexOrEnd("1[2-4]+".r)

    val endsInTerminator = "abcd13"
    assertEquals(
      parser.parse(endsInTerminator),
      Result.Success("abcd", endsInTerminator, 0, 4)
    )

    val endsWithoutTerminator1 = "abcd"
    assertEquals(
      parser.parse(endsWithoutTerminator1),
      Result.Continue(endsWithoutTerminator1, endsWithoutTerminator1, 0)
    )

    val endsWithoutTerminator2 = "abcd15"
    assertEquals(
      parser.parse(endsWithoutTerminator2),
      Result.Continue(endsWithoutTerminator2, endsWithoutTerminator2, 0)
    )

    val endsAtFirstMatch = "abcd12ef12"
    assertEquals(
      parser.parse(endsAtFirstMatch),
      Result.Success("abcd", endsAtFirstMatch, 0, 4)
    )

    val endsImmediately = "12"
    assertEquals(
      parser.parse(endsImmediately),
      Result.Success("", endsImmediately, 0, 0)
    )
  }

  test("Parser0.charsUntilTerminator") {
    assertEquals(
      Parser0.charsUntilTerminator("abc").parse("123abc"),
      Result.Success("123", "123abc", 0, 3)
    )
    assertEquals(
      Parser0.charsUntilTerminator("a", "bc").parse("123abc"),
      Result.Success("123", "123abc", 0, 3)
    )
    assertEquals(
      Parser0.charsUntilTerminator("a", "b").parse("123b"),
      Result.Success("123", "123b", 0, 3)
    )
    assertEquals(
      Parser0.charsUntilTerminator("abc").parse("123"),
      Result.Committed("123", 0, 3)
    )
    assertEquals(
      Parser0.charsUntilTerminator("abc").parse("abc"),
      Result.Success("", "abc", 0, 0)
    )
  }

  test("Parser0.charsUntilTerminatorOrEnd") {
    assertEquals(
      Parser0.charsUntilTerminatorOrEnd("\n").parse("123abc"),
      Result.Continue("123abc", "123abc", 0)
    )
    assertEquals(
      Parser0.charsUntilTerminatorOrEnd("\n").parse("123abc\n"),
      Result.Success("123abc", "123abc\n", 0, 6)
    )
    assertEquals(
      Parser0.charsUntilTerminatorOrEnd(">", "<").parse("123<"),
      Result.Success("123", "123<", 0, 3)
    )
    assertEquals(
      Parser0.charsUntilTerminatorOrEnd(">", "<").parse("123>"),
      Result.Success("123", "123>", 0, 3)
    )
    assertEquals(
      Parser0.charsUntilTerminatorOrEnd(">", "<").parse("123"),
      Result.Continue("123", "123", 0)
    )
    assertEquals(
      Parser0.charsUntilTerminatorOrEnd("abc").parse("abc"),
      Result.Success("", "abc", 0, 0)
    )
  }
}
