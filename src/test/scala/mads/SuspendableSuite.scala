package mads

import cats.data._
import cats.implicits.*
import munit.FunSuite

class SuspendableSuite extends FunSuite {
  test("Parser.commit uses underlying parser") {
    assertEquals(
      Parser.char('a').commit.parse("a"),
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
      parser.parseToCompletion(List("abc ", "do ray me ", "you and me", " 123"))

    assertEquals(
      result,
      Suspendable.Result.Success("abc do ray me you and me ", " 123", 0, 1)
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
      parser
        .parse("    ")
        .injectAndCompleteOrRestart("inject", "", parser)
        .unsafeGet

    assertEquals(result, "inject")
  }

  test("Suspendable.orElse succeeds if left succeeds") {
    val left = Parser.string("left").commit[String]
    val right = Parser.string("right").commit[String]
    val parser = left.orElse(right)

    assertEquals(
      parser.parseToCompletion(List("left")),
      Suspendable.Result.Success("left", "left", 0, 4)
    )
  }

  test("Suspendable.orElse succeeds if right succeeds") {
    val left = Parser.string("left").commit[String]
    val right = Parser.string("right").commit[String]
    val parser = left.orElse(right)

    assertEquals(
      parser.parseToCompletion(List("right")),
      Suspendable.Result.Success("right", "right", 0, 5)
    )
  }

  test("Suspendable.orElse succeeds if resumable left succeeds") {
    val left = Parser.charsWhile(_.isDigit).resume
    val right = Parser.charsWhile(_.isWhitespace).resume
    val parser = left.orElse(right)

    assertEquals(
      parser.parseToCompletion(List("1234")),
      Suspendable.Result.Success("1234", "1234", 0, 4)
    )
  }

  test("Suspendable.orElse succeeds if resumable right succeeds") {
    val left = Parser.charsWhile(_.isDigit).resume
    val right = Parser.charsWhile(_.isWhitespace).resume
    val parser = left.orElse(right)

    assertEquals(parser.complete("   ").unsafeGet, "   ")
  }

  test("Suspendable.rep repeats until parser does not succeed") {
    val parser = Parser.char('a').commit.rep
    val input = "aaaa "

    assertEquals(
      parser.parse(input),
      Resumable.success(NonEmptyChain('a', 'a', 'a', 'a'), input, 0, 4)
    )
  }

  test("Suspendable.rep accumulates results in correct order") {
    val parser = Parser.charWhere(_.isDigit).commit.rep
    val input = "1234 "

    assertEquals(
      parser.parse(input),
      Resumable.success(NonEmptyChain('1', '2', '3', '4'), input, 0, 4)
    )
  }

  test("Suspendable.rep fails if parser doesn't parse at least once") {
    val parser = Parser.charWhere(_.isDigit).commit.rep
    val input = " "

    assertEquals(parser.parse(input), Resumable.epsilon(input, 0))
  }

  test("Suspendable.rep succeeds if parser parses at least once") {
    val parser = Parser.charWhere(_.isDigit).commit.rep
    val input = "1 "

    assertEquals(
      parser.parse(input),
      Resumable.success(NonEmptyChain('1'), input, 0, 1)
    )
  }

  test("Suspendable.rep parses across suspension") {
    val parser = Parser.charsWhile(_.isDigit).resume.rep.map(_.combineAll)
    val input = NonEmptyChain("1111", "2222", "3333")

    val result = parser.parseToCompletion(input.toList)

    assertEquals(
      result,
      Suspendable.Result.Success(input.combineAll, "3333", 0, 4)
    )
  }

  test("Suspendable.rep and .orElse resumes across suspension") {
    val p1 = Parser.charsWhile(_.isDigit)
    val p2 = Parser.charsWhile(_.isSpaceChar)

    val parser = p1.orElse(p2).resume.rep.map(_.combineAll)
    val input = List("1111", "    ", "2222", "3333", "    ", "    ", "4444a")
    val expected = "1111    22223333        4444"

    val result = parser.parseToCompletion(input)

    assertEquals(result, Suspendable.Result.Success(expected, "4444a", 0, 4))
  }
}
