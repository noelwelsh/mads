package mads

import cats.data.Chain

trait Parser[A](repr: Representation[A]):
  import ParserState._

  def loop(parts: Array[String], args: Array[Any], inputState: InputState, parserState: ParserState[A]): A =
    ???

  def parse(parts: Array[String], args: Array[Any]): A =
    ???
object Parser:
  enum BlockType:
    case H1
    case H2
    case H3
    case H4
    case H5
    case H6
    case P

  // The parser's internal state
  enum ParserState[A]:
    case Block(blockType: BlockType, content: Chain[String & A])
    /** Parser is outside a block */
    case Outside()

  final case class InputState(partIndex: Int, partOffset: Int, argIndex: Int)
