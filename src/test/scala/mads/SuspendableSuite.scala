package mads

import munit.FunSuite

class SuspendableSuite extends FunSuite {
  test("Unsuspendable parser uses underlying parser") {
    assertEquals(
      Suspendable.fromParser(Parser.char('a')).parse("a"),
      Resumable.success('a', "a", 0, 1)
    )
  }

  test(
    "Suspendable parser suspends if underlying parser continues"
  ) {
    val parser = Parser.charsUntilTerminatorOrEnd("123").resume
    val result = parser.parse("abc")
    assert(result.isSuspension, result)
  }

  test("Suspendable parser consumes all parseable input across suspension") {
    val parser = Parser.charsUntilTerminatorOrEnd("123").resume
    val result =
      parser.parseToCompletion(List("abc ", "do ray me ", "you and me", "123"))

    assertEquals(
      result,
      Suspendable.Result.Success("abc do ray me you and me", "123", 0, 3)
    )
  }

  test("Advance proceeds to next parser when input ends") {
    val parser = Parser.charsWhile(_.isWhitespace).advance[String] *> Parser
      .charsUntil(_.isWhitespace)
      .resume
    val result =
      parser.parse("    ").injectAndResumeOrRestart("inject", "Doggie ", parser)

    assertEquals(result, Resumable.success("injectDoggie", "Doggie ", 0, 6))
  }

  test(
    "Advance proceeds to next parser when input ends + input ends after advance"
  ) {
    val parser = Parser.charsWhile(_.isWhitespace).advance[String] *> Parser
      .charsUntilTerminatorOrEnd("<")
      .resume
    val result =
      parser.parse("    ").injectAndResumeOrRestart("inject", "", parser).get

    assertEquals(result, Some("inject"))
  }

  test("Suspendable.orElse succeeds if left succeeds") {
    val left = Suspendable.fromParser(Parser.string("left"))
    val right = Suspendable.fromParser(Parser.string("right"))
    val parser = left.orElse(right)

    assertEquals(
      parser.parseToCompletion(List("left")),
      Suspendable.Result.Success("left", "left", 0, 4)
    )
  }

  test("Suspendable.orElse succeeds if right succeeds") {
    val left = Suspendable.fromParser(Parser.string("left"))
    val right = Suspendable.fromParser(Parser.string("right"))
    val parser = left.orElse(right)

    assertEquals(
      parser.parseToCompletion(List("right")),
      Suspendable.Result.Success("right", "right", 0, 5)
    )
  }
}
