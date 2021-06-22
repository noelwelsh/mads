package mads

import cats.data.Chain

enum Parser[A] {
  import Parser._

  def *>[B](that: Parser[B]): Parser[B] =
    (this.void ~ that).map(_.apply(1))

  def <*[B](that: Parser[B]): Parser[A] =
    (this ~ that.void).map(_.apply(0))

  def ~[B](that: Parser[B]): Parser[(A, B)] =
    Product(this, that)

  def as[B](b: B): Parser[B] =
    this.map(_ => b)

  def map[B](f: A => B): Parser[B] =
    Map(this, f)

  def orElse(that: Parser[A]): Parser[A] =
    OrElse(this, that)

  def resumable: Parser[Chain[A]] =
    Resumable(this)

  def void: Parser[Unit] =
    Void(this)


  def parseFrom(input: String, offset: Int): Result[A] =
    if offset >= input.size then Result.Epsilon(offset)
    else
      this match {
        case Character(ch) =>
          if input(offset) == ch then Result.Parsed(ch, offset + 1)
          else Result.Epsilon(offset)

        case CharacterWhere(p) =>
          val ch = input(offset)
          if p(ch) then Result.Parsed(ch, offset + 1)
          else Result.Epsilon(offset)

        case CharactersWhile(p) =>
          def loop(offset: Int): Int =
            if p(input(offset)) then loop(offset + 1)
            else offset

          val end = loop(offset)
          if offset == end then Result.Epsilon(end)
          else Result.Parsed(input.substring(offset, end), end)

        case CharactersUntilTerminator(ts) =>
          var idx = -1
          var nextOffset = -1
          ts.foreach{ t =>
            val i = input.indexOf(t, offset)
            if (i != -1) && ((idx == -1) || (i < idx)) then
              idx = i
              nextOffset = idx + t.size
            ()
          }
          if idx == -1 then Result.Committed(input.size)
          else Result.Parsed(input.substring(offset, idx), nextOffset)

        case Exactly(s) =>
          if input.startsWith(s, offset) then Result.Parsed(s, offset + s.size)
          else Result.Epsilon(offset)

        case Map(s, f) =>
          s.parseFrom(input, offset) match {
            case Result.Epsilon(o) => Result.Epsilon(o)
            case Result.Committed(o) => Result.Committed(o)
            case Result.Interrupted(r) => Result.Interrupted(r)
            case Result.Parsed(r, o) => Result.Parsed(f(r), o)
          }
      }

  def parse(input: String): Result[A] = {
    parseFrom(input, 0)
  }

  case Character(char: Char) extends Parser[Char]
  case CharacterWhere(predicate: Char => Boolean) extends Parser[Char]
  case CharactersWhile(predicate: Char => Boolean) extends Parser[String]
  /** Match until the first example of one of the terminators */
  case CharactersUntilTerminator(terminators: Seq[String]) extends Parser[String]
  case Exactly(expected: String) extends Parser[String]
  case Map[A,B](source: Parser[A], f: A => B) extends Parser[B]
  case OrElse(left: Parser[A], right: Parser[A])
  case OneOf(parsers: List[Parser[A]])
  case Product[A,B](left: Parser[A], right: Parser[B]) extends Parser[(A,B)]
  case Resumable(source: Parser[A]) extends Parser[Chain[A]]
  case StringsIn(strings: Iterable[String]) extends Parser[String]
  case Succeed()
  case Void(parser: Parser[A]) extends Parser[Unit]
}
object Parser {
  def char(char: Char): Parser[Char] =
    Parser.Character(char)

  def charWhere(predicate: Char => Boolean): Parser[Char] =
    Parser.CharacterWhere(predicate)

  def charsWhile(predicate: Char => Boolean): Parser[String] =
    Parser.CharactersWhile(predicate)

  def charsUntil(predicate: Char => Boolean): Parser[String] =
    charsWhile(ch => !predicate(ch))

  def charsUntilTerminator(terminators: String*): Parser[String] =
    CharactersUntilTerminator(terminators)

  def oneOf[A](parsers: List[Parser[A]]): Parser[A] =
    OneOf(parsers)

  def string(string: String): Parser[String] =
    Exactly(string)

  def stringsIn(strings: Iterable[String]): Parser[String] =
    StringsIn(strings)

  def succeed: Parser[Unit] =
    Succeed()



  sealed trait Resumption[A](parser: Parser[Chain[A]]) {
    def resume(unparsed: A): Parser[Chain[A]]
  }

  enum Result[A] {
    // Parsed nothing up to one character before the given offset and failed
    case Epsilon(offset: Int)
    // Parsed up to one character before the given offset and failed
    case Committed(offset: Int)
    // Successfully parsed all input but was expecting additional input
    // part is the input
    // parser is the parser that should be resumed
    case Interrupted(resumption: Resumption[A])
    // Successfully parsed input up one character before the given to offset as result
    case Parsed(result: A, offset: Int)
  }
}
