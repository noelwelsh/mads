package mads

import munit.FunSuite

class SuspendableSuite extends FunSuite {
  test("Unsuspendable parser uses underlying parser") {
    assertEquals(
      Suspendable.fromParser(Parser.char('a')).parse("a"),
      Resumable.success('a', "a", 1)
    )
  }
}
