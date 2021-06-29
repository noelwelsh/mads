package mads

import cats.Semigroup
import cats.data.Chain

/** A parser that produces a value of type A.
  *
  * The context is used to represent the possibility of resuming the parser with
  * additional input and, optionally, already parsed input. There are two
  * context types provided:
  *
  *   - `Complete` which is the result type for parsers that cannot be resumed.
  *   - `Resumable`, which is the result type for parsers that can be resumed.
  *
  * There are three cases within `Complete`:
  *
  *   - `Epsilon`, for a parser that failed but did not consume any input;
  *   - `Committed`, for a parser that failed after consuming some input; and
  *   - `Success`, for a parser that successfully parsed a portion of it's input
  *     and produced a value of type `A`.
  *
  * `Resumable` adds an additional cause for a parser that can be resumed.
  *
  * Parsers can be resumed if they accept their complete input but do not parse
  * it. In other words, a parser that `Committed` to its entire input.
  * Additionally, the programmer must indicate that a parser can be resumed
  * using the `resumable` method.
  *
  * Resumption is quite limited. The original input is not reparsed on
  * resumption. Parsing starts again on the new input and hence a parser cannot
  * carry state across resumption boundaries.
  */
enum Parser[A] {
  import Parser._

  def *>[B](that: Parser[B]): Parser[B] =
    (this.void ~ that).map(_.apply(1))

  def *>[S, B](that: Suspendable[S, B]): Suspendable[S, B] =
    (this.void ~ that).map(_.apply(1))

  def <*[B](that: Parser[B]): Parser[A] =
    (this ~ that.void).map(_.apply(0))

  def <*[S, B](that: Suspendable[S, B]): Suspendable[S, A] =
    (this ~ that.void).map(_.apply(0))

  def ~[B](that: Parser[B]): Parser[(A, B)] =
    Product(this, that)

  def ~[S, B](that: Suspendable[S, B]): Suspendable[S, (A, B)] =
    Suspendable.fromParser(this) ~ that

  def as[B](b: B): Parser[B] =
    this.map(_ => b)

  def map[B](f: A => B): Parser[B] =
    Map(this, f)

  def orElse(that: Parser[A]): Parser[A] =
    OrElse(this, that)

  def suspendable(using
      semigroup: Semigroup[A],
      ev: String =:= A
  ): Suspendable[A, A] =
    Suspendable.Suspend(this, str => ev(str), semigroup)

  def suspendable(f: String => A)(using
      semigroup: Semigroup[A]
  ): Suspendable[A, A] =
    Suspendable.Suspend(this, f, semigroup)

  def void: Parser[Unit] =
    Void(this)

  def parse(input: String, offset: Int = 0): Complete[A] = {
    import Complete._
    import Parser._

    if offset >= input.size then Epsilon(input, offset)
    else
      this match {
        case Character(ch) =>
          if input(offset) == ch then Success(ch, input, offset + 1)
          else Epsilon(input, offset)

        case CharacterWhere(p) =>
          val ch = input(offset)
          if p(ch) then Success(ch, input, offset + 1)
          else Epsilon(input, offset)

        case CharactersWhile(p) =>
          def loop(offset: Int): Int =
            if offset == input.size then offset
            else if p(input(offset)) then loop(offset + 1)
            else offset

          val end = loop(offset)
          Success(input.substring(offset, end), input, end)

        case CharactersUntilTerminator(ts) =>
          var idx = -1
          var nextOffset = -1
          ts.foreach { t =>
            val i = input.indexOf(t, offset)
            if (i != -1) && ((idx == -1) || (i < idx)) then
              idx = i
              nextOffset = idx + t.size
            ()
          }
          if idx == -1 then Committed(input, input.size)
          else Success(input.substring(offset, idx), input, nextOffset)

        case CharactersUntilTerminatorOrEnd(ts) =>
          var idx = -1
          var nextOffset = -1
          ts.foreach { t =>
            val i = input.indexOf(t, offset)
            if (i != -1) && ((idx == -1) || (i < idx)) then
              idx = i
              nextOffset = idx + t.size
            ()
          }
          if idx == -1 then Success(input.substring(offset), input, input.size)
          else Success(input.substring(offset, idx), input, nextOffset)

        case Exactly(s) =>
          if input.startsWith(s, offset) then Success(s, input, offset + s.size)
          else Epsilon(input, offset)

        case Map(s, f) =>
          s.parse(input, offset) match {
            case Epsilon(i, o)    => Epsilon(i, o)
            case Committed(i, o)  => Committed(i, o)
            case Success(a, i, o) => Success(f(a), i, o)
          }

        case OrElse(l, r) =>
          l.parse(input, offset) match {
            case Epsilon(i, o)    => r.parse(i, o)
            case Committed(i, o)  => Committed(i, o)
            case Success(r, i, o) => Success(r, i, o)
          }

        case OneOf(parsers) =>
          def loop[A](parsers: List[Parser[A]]): Complete[A] =
            parsers match {
              case Nil => Epsilon(input, offset)
              case p :: ps =>
                p.parse(input, offset) match {
                  case Epsilon(i, o)    => loop(ps)
                  case Committed(i, o)  => Committed(i, o)
                  case Success(r, i, o) => Success(r, i, o)
                }
            }

          loop(parsers)

        case Product(l, r) =>
          l.parse(input, offset) match {
            case Epsilon(i, o)   => Epsilon(i, o)
            case Committed(i, o) => Committed(i, o)
            case Success(a, i, o) =>
              r.parse(input, offset) match {
                case Epsilon(i, o)    => Epsilon(i, o)
                case Committed(i, o)  => Committed(i, o)
                case Success(b, i, o) => Success((a, b), i, o)
              }
          }

        case StringIn(s) =>
          var found = false
          var length = -1
          var matched: String = null

          s.foreach(str =>
            if (str.size > length) && input.startsWith(str, offset) then
              found = true
              matched = str
              length = str.size
          )
          if found then Success(matched, input, offset + length)
          else Epsilon(input, offset)

        case Void(p) =>
          p.parse(input, offset) match {
            case Epsilon(i, o)    => Epsilon(i, o)
            case Committed(i, o)  => Committed(i, o)
            case Success(a, i, o) => Success((), i, o)
          }
      }
  }

  def parseOrExn(input: String): A =
    this.parse(input, 0) match {
      case Complete.Success(a, _, _) => a
      case other => throw new Exception(s"Parsing failed with $other")
    }

  case Character(char: Char) extends Parser[Char]
  case CharacterWhere(predicate: Char => Boolean) extends Parser[Char]
  case CharactersWhile(predicate: Char => Boolean) extends Parser[String]
  case CharactersUntilTerminator(terminators: Seq[String])
      extends Parser[String]
  case CharactersUntilTerminatorOrEnd(terminators: Seq[String])
      extends Parser[String]
  case Exactly(expected: String) extends Parser[String]
  case Product[A, B](left: Parser[A], right: Parser[B]) extends Parser[(A, B)]
  case Map[A, B](source: Parser[A], f: A => B) extends Parser[B]
  case OrElse(left: Parser[A], right: Parser[A]) extends Parser[A]
  case OneOf(parsers: List[Parser[A]]) extends Parser[A]
  case StringIn(strings: Iterable[String]) extends Parser[String]
  case Void(parser: Parser[A]) extends Parser[Unit]
}
object Parser {
  def char(char: Char): Parser[Char] =
    Parser.Character(char)

  def charWhere(predicate: Char => Boolean): Parser[Char] =
    Parser.CharacterWhere(predicate)

  /** Parses zero or more character until the predicate succeeds.
    */
  def charsWhile(predicate: Char => Boolean): Parser[String] =
    Parser.CharactersWhile(predicate)

  def charsUntil(predicate: Char => Boolean): Parser[String] =
    charsWhile(ch => !predicate(ch))

  /** Parse until the first example of one of the terminators */
  def charsUntilTerminator(terminators: String*): Parser[String] =
    CharactersUntilTerminator(terminators)

  /** Parse until the first example of one of the terminators or the end of the
    * input
    */
  def charsUntilTerminatorOrEnd(terminators: String*): Parser[String] =
    CharactersUntilTerminatorOrEnd(terminators)

  def oneOf[A](parsers: List[Parser[A]]): Parser[A] =
    OneOf(parsers)

  /** Parse exactly the given string */
  def string(string: String): Parser[String] =
    Exactly(string)

  /** Parse the longest matching string amongst the given strings */
  def stringIn(strings: Iterable[String]): Parser[String] =
    StringIn(strings)
}
