package mads

import cats.implicits._

object TextRepresentation extends Representation[String] {
  def h1(header: String): String = s"<h1>$header</h1>"
  def h2(header: String): String = s"<h2>$header</h2>"
  def h3(header: String): String = s"<h3>$header</h3>"
  def h4(header: String): String = s"<h4>$header</h4>"
  def h5(header: String): String = s"<h5>$header</h5>"
  def h6(header: String): String = s"<h6>$header</h6>"

  def paragraph(content: String): String = content

  def argument(a: Any): Either[String, String] = a.toString.asRight

  def text(text: String): String = text
}
