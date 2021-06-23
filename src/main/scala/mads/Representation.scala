package mads

trait Representation[A]:
  def h1(header: A): A
  def h2(header: A): A
  def h3(header: A): A
  def h4(header: A): A
  def h5(header: A): A
  def h6(header: A): A

  def paragraph(content: A): A

  def argument(a: Any): Either[String, A]

  def text(text: String): A
