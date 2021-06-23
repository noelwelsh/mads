package mads

import munit.FunSuite

class MadsSuite extends FunSuite {
  val mads = Mads(TextRepresentation)

  test("h1") {
    val input = "# Heading 1\n"
    assertEquals(mads.parse(Array(input), Array()), Parser.Result.Parsed("<h1>Heading 1</h1>", input, input.size))
  }

}
