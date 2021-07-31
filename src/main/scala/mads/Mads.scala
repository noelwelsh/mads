package mads

import cats.{Monoid, Semigroup}
import cats.data.Chain
import cats.implicits._
import cats.instances.string

final case class Mads[A](repr: Representation[A])(using monoid: Monoid[A]) {
  import Mads._
  import Suspendable._

  object syntax {
    extension (sc: StringContext) {
      def md(args: Any*): A = {
        parse(sc.parts, args).getOrExn
      }
    }
  }

  val lineEnd: Parser[Unit] =
    Parser.string("\r\n").orElse(Parser.string("\n")).void
  val whiteSpace0: Parser[Unit] =
    Parser0.charsWhile(ch => ch == ' ' || ch == '\t').void
  val whiteSpace1: Parser[Unit] =
    Parser.charsWhile(ch => ch == ' ' || ch == '\t').void
  val emptyLine: Parser[A] = (whiteSpace0 *> lineEnd).map(_ => monoid.empty)

  val hash = Parser.char('#')

  val heading: Suspendable[A, A] = {
    val level =
      Parser.stringIn(List("#", "##", "###", "####", "#####", "######"))

    val content: Suspendable[A, A] =
      whiteSpace1.advance[A] *> Parser0
        .charsUntilTerminatorOrEnd("\n", "\r\n")
        .map(repr.text)
        .resumeWith(repr.text)

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

  val paragraph: Suspendable[A, A] =
    Parser
      // A paragraph ends at a blank line or the end of the input
      .charsUntilRegexOrEnd(raw"\v\w*\v".r)
      .map(repr.text)
      .resumeWith(repr.text)
      .map(repr.paragraph)


  val code: Suspendable[A, A] = {
    val start = Parser.string("```") *> Parser.charsUntilTerminator("\n", "\r\n") <* lineEnd
    val content = Parser0.charsUntilRegexOrEnd(raw"\v```".r).map(repr.text).resumeWith(repr.text) <* lineEnd.commit
    val stop = (Parser.string("```") ~ whiteSpace0 ~ lineEnd.orElse(Parser.end.void)).void.commit[A]

    (start ~ stop.map(_ => repr.text("")).orElse(content <* stop)).map((language, code) => repr.code(language, code))
  }

  val parser: Suspendable[A, A] =
    emptyLine.commit.backtrack
      .orElse(heading.backtrack)
      .orElse(code.backtrack)
      .orElse(paragraph.backtrack)
      .rep
      .map(_.combineAll)

  def parse(parts: Seq[String], args: Seq[Any]): Resumable[A, A] = {
    def loop(idx: Int, result: Resumable[A, A]): Resumable[A, A] =
      if idx >= parts.size then result
      else
        val arg = args(idx - 1)
        val part = parts(idx)
        val a = repr.argument(args(idx - 1))
        loop(idx + 1, result.injectAndResumeOrRestart(a, part, parser))

    assert(
      parts.size > 0,
      "Mads cannot parse this input. There were no string parts to parse. There must be at least one."
    )
    assert(
      args.size == parts.size - 1,
      s"Mads cannot parse this input. There must be one less argument than string parts. There were ${parts.size} string parts and ${args.size} arguments"
    )

    loop(1, parser.parse(parts(0)))
  }
}
object Mads {
  val text = Mads(TextRepresentation)
}
