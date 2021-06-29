package mads

import munit.FunSuite

class MadsSuite extends FunSuite {
  val mads = Mads(TextRepresentation)

  def parse(input: List[String], args: List[String] = Nil): Option[String] =
    mads.parse(input.toArray, args.toArray[Any])

  test("h1 with newline") {
    val input = List("# Heading 1\n")
    assertEquals(
      parse(input),
      Some("<h1>Heading 1</h1>")
    )
  }

  test("h1 without newline") {
    val input = List("# Heading 1")
    assertEquals(
      parse(input),
      Some("<h1>Heading 1</h1>")
    )
  }

  test("h1 with arg and newline") {
    val input = List("# ", "\n")
    val args  = List("Heading 1")

    val result = parse(input, args)

    assertEquals(result, Some("<h1>Heading 1</h1>"))
  }

  test("h1 with arg and no newline") {
    val input = List("# ", "")
    val args  = List("Heading 1")

    val result = parse(input, args)

    assertEquals(result, Some("<h1>Heading 1</h1>"))
  }

  test("p") {
    val input = List("This is mad!\n")

    val result = parse(input)
    assertEquals(result, Some("<p>This is mad!</p>"))
  }
}
