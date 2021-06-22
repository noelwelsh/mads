package mads

import cats.data.Chain
import cats.implicits._
import cats.instances.string

final case class Mads[A](repr: Representation[A]) {
  import Mads._

  val lineEnd: Parser[Unit] = Parser.string("\r\n").orElse(Parser.string("\n")).void
  val whiteSpace: Parser[Unit] = Parser.charsWhile(_.isWhitespace).void
  val emptyLine: Parser[Unit] = (whiteSpace ~ lineEnd).void

  val hash = Parser.char('#')

  val heading: Parser[Heading[A]] = {
    val level =
      Parser.stringsIn(List("#", "##", "###", "####", "#####", "######"))
        .map(string =>
          string match {
            case "#" => HeadingLevel.H1
            case "##" => HeadingLevel.H2
            case "###" => HeadingLevel.H3
            case "####" => HeadingLevel.H4
            case "#####" => HeadingLevel.H5
            case "######" => HeadingLevel.H6
          }
        )
    val content =
      whiteSpace *> Parser.charsUntilTerminator("\n", "\r\n").map(repr.text).resumable

    (level ~ content).map((l, c) => Heading(l, c))
  }
}
object Mads {
  enum Element {
    case Part(part: String)
    case Argument(arg: Any)
    case Eof
  }

  final case class Heading[A](level: HeadingLevel, content: Chain[A])
  enum HeadingLevel {
    case H1
    case H2
    case H3
    case H4
    case H5
    case H6
  }

  final case class InputState(parts: Array[String], args: Array[Any], partIndex: Int, argIndex: Int, inPart: Boolean) {
    def next: (InputState, Element) =
      if inPart && (partIndex < parts.size) then
        val elt = parts(partIndex)
        val next = this.copy(partIndex = partIndex + 1, inPart = !inPart)
        (next, Element.Part(elt))
      else if !inPart && (argIndex < args.size) then
        val elt = args(argIndex)
        val next = this.copy(argIndex = argIndex + 1, inPart = !inPart)
        (next, Element.Argument(elt))
      else
        (this, Element.Eof)
  }

}
