package mads

import cats.data._
import munit.FunSuite

class SuspendableSuite extends FunSuite {
  test("Parser.unsuspendable uses underlying parser") {
    assertEquals(
      Parser.char('a').unsuspendable.parse("a"),
      Resumable.success('a', "a", 0, 1)
    )
  }

  test(
    "Parser.resume suspends if underlying parser continues"
  ) {
    val parser = Parser.charsUntilTerminatorOrEnd("123").resume
    val result = parser.parse("abc")
    assert(result.isSuspension, result)
  }

  test("Parser.resume consumes all parseable input across suspension") {
    val parser = Parser.charsUntilTerminatorOrEnd("123").resume
    val result =
      parser.parseToCompletion(List("abc ", "do ray me ", "you and me", "123"))

    assertEquals(
      result,
      Suspendable.Result.Success("abc do ray me you and me", "123", 0, 3)
    )
  }

  test("Parser.advance proceeds to next parser when input ends") {
    val parser = Parser.charsWhile(_.isWhitespace).advance[String] *> Parser
      .charsUntil(_.isWhitespace)
      .resume
    val result =
      parser.parse("    ").injectAndResumeOrRestart("inject", "Doggie ", parser)

    assertEquals(result, Resumable.success("injectDoggie", "Doggie ", 0, 6))
  }

  test(
    "Parser.advance proceeds to next parser when input ends + input ends after advance"
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

  test("Suspendable.rep repeats until parser does not succeed") {
    val parser = Parser.char('a').unsuspendable.rep
    val input = "aaaa "

    assertEquals(
      parser.parse(input),
      Resumable.success(NonEmptyChain('a', 'a', 'a', 'a'), input, 0, 4)
    )
  }

  test("Suspendable.rep accumulates results in correct order") {
    val parser = Parser.charWhere(_.isDigit).unsuspendable.rep
    val input = "1234 "

    assertEquals(
      parser.parse(input),
      Resumable.success(NonEmptyChain('1', '2', '3', '4'), input, 0, 4)
    )
  }

  test("Suspendable.rep fails if parser doesn't parse at least once") {
    val parser = Parser.charWhere(_.isDigit).unsuspendable.rep
    val input = " "

    assertEquals(parser.parse(input), Resumable.epsilon(input, 0))
  }

  test("Suspendable.rep succeeds if parser parses at least once") {
    val parser = Parser.charWhere(_.isDigit).unsuspendable.rep
    val input = "1 "

    assertEquals(
      parser.parse(input),
      Resumable.success(NonEmptyChain('1'), input, 0, 1)
    )
  }
}
