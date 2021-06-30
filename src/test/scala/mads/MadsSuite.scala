package mads

import munit.FunSuite

class MadsSuite extends FunSuite {
  val mads = Mads(TextRepresentation)

  test("h1") {
    val input = "# Heading 1\n"

    val result = mads.parse(Array(input), Array())
    assertEquals(
      result.get,
      Some("<h1>Heading 1</h1>"),
      result
    )
  }

  test("h1 with arg") {
    val input = Array("# ", "\n")
    val args: Array[Any] = Array("Heading 1")

    val result = mads.parse(input, args)

    assertEquals(result.get, Some("<h1>Heading 1</h1>"), result)
  }
}
