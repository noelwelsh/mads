package mads

import cats.{Monoid, Semigroup}
import cats.data.Chain
import cats.implicits._
import cats.instances.string

final case class Mads[A](repr: Representation[A])(using monoid: Monoid[A]) {
  import Mads._
  import Suspendable._

  val lineEnd: Parser[Unit] =
    Parser.string("\r\n").orElse(Parser.string("\n")).void
  val whiteSpace: Parser[Unit] =
    Parser.charsWhile(ch => ch == ' ' || ch == '\t').void
  val emptyLine: Parser[A] = (whiteSpace *> lineEnd).map(_ => monoid.empty)

  val hash = Parser.char('#')

  val heading: Suspendable[A, A] = {
    val level =
      Parser.stringIn(List("#", "##", "###", "####", "#####", "######"))

    val content: Suspendable[A, A] =
      whiteSpace.advance[A] *> Parser
        .charsUntilTerminatorOrEnd("\n", "\r\n")
        .map(repr.text)
        .resumeWith(repr.text) <* Parser0.charsThroughTerminatorOrEnd(
        "\n",
        "\r\n"
      )

    (level ~ content).map((l, c) =>
      l match {
        case "#"      => repr.h1(c)
        case "##"     => repr.h2(c)
        case "###"    => repr.h3(c)
        case "####"   => repr.h4(c)
        case "#####"  => repr.h5(c)
        case "######" => repr.h6(c)
      }
    )
  }

  val paragraph: Suspendable[A, A] = {
    Parser
      // A paragraph ends at a blank line or the end of the input
      .charsUntilRegexOrEnd(raw"\v\w*\v".r)
      .map(repr.text)
      .resumeWith(repr.text)
      .map(repr.paragraph)
  }

  val parser: Suspendable[A, A] =
    emptyLine.unsuspendable.backtrack
      .orElse(heading.backtrack)
      .orElse(paragraph.backtrack)
      .rep
      .map(_.combineAll)

  def parse(parts: Array[String], args: Array[Any]): Resumable[A, A] = {
    def loop(idx: Int, result: Resumable[A, A]): Resumable[A, A] =
      if idx >= parts.size then result
      else
        val a = repr.argument(args(idx - 1))
        val input = parts(idx)
        loop(idx + 1, result.injectAndResumeOrRestart(a, input, parser))

    assert(parts.size > 0)
    assert(args.size == parts.size - 1)

    loop(1, parser.parse(parts(0)))
  }
}
object Mads {
  val madsText = Mads(TextRepresentation)
}
