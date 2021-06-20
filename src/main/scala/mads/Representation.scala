package mads

trait Representation[A]:
  def h1(header: String): A
  def h2(header: String): A
  def h3(header: String): A
  def h4(header: String): A
  def h5(header: String): A
  def h6(header: String): A

  def paragraph(content: String & A): A
