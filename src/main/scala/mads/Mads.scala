package mads

import cats.Semigroup
import cats.data.Chain
import cats.implicits._
import cats.instances.string

final case class Mads[A: Semigroup](repr: Representation[A]) {
  import Mads._
  import Suspendable._

  val lineEnd: Parser[Unit] =
    Parser.string("\r\n").orElse(Parser.string("\n")).void
  val whiteSpace: Parser[Unit] = Parser.charsWhile(_.isWhitespace).void
  val emptyLine: Parser[Unit] = (whiteSpace ~ lineEnd).void

  val hash = Parser.char('#')

  val heading: Suspendable[A, A] = {
    val level =
      Parser.stringIn(List("#", "##", "###", "####", "#####", "######"))

    val content: Suspendable[A, A] =
      whiteSpace *> Parser
        .charsUntilTerminator("\n", "\r\n")
        .map(repr.text)
        .suspendable(repr.text)

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

  val parser: Suspendable[A, A] = heading

  def parse(parts: Array[String], args: Array[Any]): Complete[A] = {
    import Complete.*
    import Resumable.*

    def loop(idx: Int, result: Resumable[A, A]): Complete[A] =
      result match {
        case Finished(Success(a1, i, o)) =>
          if idx >= parts.size then Complete.Success(a1, i, o)
          else
            // We successfully parsed part of the file and will attempt to continue parsing. A manual resumption
            val a2 = repr.argument(args(idx - 1))
            val i = parts(idx)
            val result = parser.parse(i).map(r2 => a1.combine(a2).combine(r2))
            loop(idx + 1, result)

        case Finished(other) => other

        case Suspended(s) =>
          if idx >= parts.size then
            Complete.Committed(parts.last, parts.last.size)
          else
            val a = repr.argument(args(idx - 1))
            val i = parts(idx)
            loop(idx + 1, s.inject(a).parse(i))
      }

    assert(parts.size > 0)
    assert(args.size == parts.size - 1)

    loop(1, parser.parse(parts(0)))
  }
}
object Mads {
  enum Element {
    case Part(part: String)
    case Argument(arg: Any)
    case Eof
  }

  final case class Heading[A](level: HeadingLevel, content: A)
  enum HeadingLevel {
    case H1
    case H2
    case H3
    case H4
    case H5
    case H6
  }

  final case class InputState(
      parts: Array[String],
      args: Array[Any],
      partIndex: Int,
      argIndex: Int,
      inPart: Boolean
  ) {
    def next: (InputState, Element) =
      if inPart && (partIndex < parts.size) then
        val elt = parts(partIndex)
        val next = this.copy(partIndex = partIndex + 1, inPart = !inPart)
        (next, Element.Part(elt))
      else if !inPart && (argIndex < args.size) then
        val elt = args(argIndex)
        val next = this.copy(argIndex = argIndex + 1, inPart = !inPart)
        (next, Element.Argument(elt))
      else (this, Element.Eof)
  }

}
