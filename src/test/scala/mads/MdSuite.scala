package mads

import cats.implicits._
import munit.FunSuite
import cats.kernel.compat.scalaVersionSpecific

class MdSuite extends FunSuite {
  val mads = Mads.text
  import mads.syntax._

  test("md string interpolation") {

    val name = "Mads"
    val emotion = "excited"

    val parsed =
      md"""
# Let's Get $name

This is $name
and it's $emotion"""

    val expected =
      s"<h1>Let's Get $name</h1><p>This is $name\nand it's $emotion</p>"
    assertEquals(parsed, expected)
  }

  test("md string interpolation in code block") {
    val name = "mads"
    val expr = "42 * 42"

    val parsed =
      md"""
```scala
val $name = $expr
```"""

    val expected =
      """<pre><code class="language-scala">val mads = 42 * 42</code></pre>"""

    assertEquals(parsed, expected)
  }
}
