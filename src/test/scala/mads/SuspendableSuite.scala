package mads

import munit.FunSuite

class SuspendableSuite extends FunSuite {
  test("Unsuspendable parser uses underlying parser") {
    assertEquals(
      Suspendable.fromParser(Parser.char('a')).parse("a"),
      Resumable.success('a', "a", 1)
    )
  }

  test(
    "Suspendable parser suspends if underlying parser commits to all input"
  ) {
    val parser = Parser.charsUntilTerminator("123").suspendable
    val result = parser.parse("abc")
    assert(result.isSuspension, result)
  }

  test("Suspendable parser consumes all parseable input across suspension") {
    val parser = Parser.charsUntilTerminator("123").suspendable
    val result =
      parser.parseToCompletion(List("abc ", "do ray me ", "you and me", "123"))

    assertEquals(result, Complete.Success("abc do ray me you and me", "123", 3))
  }
}
