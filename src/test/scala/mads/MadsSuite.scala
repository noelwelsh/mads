package mads

import cats.implicits._
import munit.FunSuite

class MadsSuite extends FunSuite {
  val mads = Mads.text

  def parse(
      input: List[String],
      args: List[String] = Nil
  ): Resumable[String, String] =
    mads.parse(input, args)

  test("h1 with newline") {
    val input = List("# Heading 1\n")
    val result = parse(input)

    assertEquals(
      parse(input).get,
      "<h1>Heading 1</h1>".some,
      result
    )
  }

  test("h1 without newline") {
    val input = List("# Heading 1")
    val result = parse(input)

    assertEquals(
      parse(input).get,
      Some("<h1>Heading 1</h1>"),
      result
    )
  }

  test("h1 with arg and newline") {
    val input = List("# ", "\n")
    val args = List("Heading 1")

    val result = parse(input, args)

    assertEquals(result.get, Some("<h1>Heading 1</h1>"), result)
  }

  test("h1 with arg and no newline") {
    val input = List("# ", "")
    val args = List("Heading 1")

    val result = parse(input, args)

    assertEquals(result.get, Some("<h1>Heading 1</h1>"), result)
  }

  test("p") {
    val input = List("This is mad!\n")

    val result = parse(input)
    assertEquals(result.get, Some("<p>This is mad!\n</p>"), result)
  }

  test("p with arg") {
    val input = List("This is ", "!")
    val args = List("Mads")

    val result = parse(input, args)
    assertEquals(result.get, Some("<p>This is Mads!</p>"), result)
  }

  test("p that spans multiple lines") {
    val input = List("""
    |How much wood
    |Would a woodchuck chuck?
    |""".stripMargin)

    val result = parse(input)
    assertEquals(
      result.get,
      Some("<p>How much wood\nWould a woodchuck chuck?\n</p>"),
      result
    )
  }

  test("Document with h1 and p") {
    val input = List("""
    |# Just A Test
    |
    |Let's test this works.
    |""".stripMargin)

    val expected =
      "<h1>Just A Test</h1><p>Let's test this works.\n</p>".stripMargin

    val result = parse(input)
    assertEquals(result.get, Some(expected))
  }

  test("Document with h1, and p with arg") {
    val input = List("""
    |# Just A Test
    |
    |Let's test """.stripMargin, " works.")

    val args = List("Mads")

    val expected = "<h1>Just A Test</h1><p>Let's test Mads works.</p>"

    val result = parse(input, args)
    assertEquals(result.get, Some(expected))
  }

  test("Document with h1 with arg, and p") {
    val input = List("# Greetings ", "\n\nTake me to your ", ".")
    val args = List("Earthlings!", "readers")

    val expected = "<h1>Greetings Earthlings!</h1><p>Take me to your readers.</p>"

    val result = parse(input, args)
    assertEquals(result.get, Some(expected))
  }

  test("Document with p and two args") {
    val greeting = "Hi!"
    val name = "Stereo Mike"

    val input = List("", " My name is ", "")
    val args = List(greeting, name)

    val expected = s"<p>$greeting My name is $name</p>"

    val result = parse(input, args)
    assertEquals(result.get, Some(expected))
  }

  test("Document with h1 and p and two args") {
    val greeting = "Hi!"
    val name = "Stereo Mike"

    val input = List("# ", "\n\nMy name is ", "")
    val args = List(greeting, name)

    val expected = s"<h1>$greeting</h1><p>My name is $name</p>"

    val result = parse(input, args)
    assertEquals(result.get, Some(expected))
  }

  test("md string interpolation") {
    import mads.syntax._

    val name = "Mads"
    val emotion = "excited"

    val parsed =
      md"""
# Let's Get $name

This is $name
and it's $emotion"""

    val expected = s"<h1>Let's get $name</h1><p>This is $name\nand it's $emotion</p>"
    assertEquals(parsed, expected)
  }
}
