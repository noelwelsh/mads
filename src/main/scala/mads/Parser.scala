package mads

import cats.Semigroup
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

  def resumable(using s: Semigroup[A], ev: String =:= A): Parser[A] =
    Resumable(this, str => ev(str), s)

  def resumable(f: String => A)(using s: Semigroup[A]): Parser[A] =
    Resumable(this, f, s)

  def void: Parser[Unit] =
    Void(this)


  def parseFrom(input: String, offset: Int): Result[A] =
    if offset >= input.size then Result.Epsilon(input, offset)
    else
      this match {
        case Character(ch) =>
          if input(offset) == ch then Result.Parsed(ch, input, offset + 1)
          else Result.Epsilon(input, offset)

        case CharacterWhere(p) =>
          val ch = input(offset)
          if p(ch) then Result.Parsed(ch, input, offset + 1)
          else Result.Epsilon(input, offset)

        case CharactersWhile(p) =>
          def loop(offset: Int): Int =
            if p(input(offset)) then loop(offset + 1)
            else offset

          val end = loop(offset)
          if offset == end then Result.Epsilon(input, end)
          else Result.Parsed(input.substring(offset, end), input, end)

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
          if idx == -1 then Result.Committed(input, input.size)
          else Result.Parsed(input.substring(offset, idx), input, nextOffset)

        case Exactly(s) =>
          if input.startsWith(s, offset) then Result.Parsed(s, input, offset + s.size)
          else Result.Epsilon(input, offset)

        case Map(s, f) =>
          s.parseFrom(input, offset).map(f)

        case OrElse(l, r) =>
          l.parseFrom(input, offset) match {
            case Result.Epsilon(i, o) => r.parseFrom(i, o)
            case Result.Committed(i, o) => Result.Committed(i, o)
            case Result.Interrupted(resume) => Result.Interrupted(resume)
            case Result.Parsed(r, i, o) => Result.Parsed(r, i, o)
          }

        case OneOf(parsers) =>
          def loop(parsers: List[Parser[A]]): Result[A] =
            parsers match {
              case Nil => Result.Epsilon(input, offset)
              case p :: ps =>
                p.parseFrom(input, offset) match {
                  case Result.Epsilon(i, o) => loop(ps)
                  case Result.Committed(i, o) => Result.Committed(i, o)
                  case Result.Interrupted(resume) => Result.Interrupted(resume)
                  case Result.Parsed(r, i, o) => Result.Parsed(r, i, o)
                }
            }

          loop(parsers)

        case p: Product[a, b] =>
          def leftLoop(input: String, offset: Int): Result[(a,b)] =
            p.left.parseFrom(input, offset) match {
              case Result.Epsilon(i, o) => Result.Epsilon(i, o)
              case Result.Committed(i, o) => Result.Committed(i, o)
              case Result.Interrupted(resume) =>
                Result.Interrupted(resume.withContinuation((a, i, o) => rightLoop(a, i, o)))
              case Result.Parsed(a, i, o) => rightLoop(a, i, o)
            }

          def rightLoop(a: a, input: String, offset: Int): Result[(a, b)] =
            p.right.parseFrom(input, offset) match {
              case Result.Epsilon(i, o) => Result.Epsilon(i, o)
              case Result.Committed(i, o) => Result.Committed(i, o)
              case Result.Interrupted(resume) => Result.Interrupted(resume.map(b => (a, b)))
              case Result.Parsed(b, i, o) => Result.Parsed((a, b), i, o)
            }

          leftLoop(input, offset)

        case Resumable(s, f, semi) =>
          s.parseFrom(input, offset) match {
            case Result.Epsilon(i, o) => Result.Epsilon(i, o)
            case Result.Committed(i, o) =>
              if o == input.size then
                Result.Interrupted(Resumption(s, f(input.substring(offset)), semi, (a, i, o) => Result.Parsed(a, i, o)))
              else Result.Committed(i, o)
            case Result.Interrupted(resume) => Result.Interrupted(resume)
            case Result.Parsed(r, i, o) => Result.Parsed(r, i, o)
          }

        case StringIn(s) =>
          val results = s.map(str => Parser.string(str).parseFrom(input, offset))
          val committed = results.collect{ case Result.Committed(i, o) => new Result.Committed[A](i, o) }
          val parsed = results.collect{ case Result.Parsed(r, i, o) => new Result.Parsed(r, i, o) }
          val interrupted = results.collect{ case Result.Interrupted(r) => new Result.Interrupted(r) }

          if interrupted.nonEmpty then ???
          else if parsed.nonEmpty then parsed.maxBy(p => p.offset)
          else if committed.nonEmpty then committed.maxBy(p => p.offset)
          else Result.Epsilon(input, offset)

        case Void(p) =>
          p.parseFrom(input, offset).map(_ => ())
      }

  def parse(input: String): Result[A] = {
    parseFrom(input, 0)
  }

  /**
   * Utility when you don't care about interruption / resumption or failures
   */
  def parseOrExn(input: String): A =
    parse(input) match {
      case Result.Parsed(r, _, _) => r
      case Result.Interrupted(_) => throw new Exception("Parsing was interrupted")
      case Result.Committed(i, o) => throw new Exception(s"Parsing failed on input ${i} after parsing up to offset ${o}")
      case Result.Epsilon(i, o) => throw new Exception(s"Parsing failed to advance on input ${i} from offset ${o}")
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
  case Resumable(source: Parser[A], f: String => A, semigroup: Semigroup[A]) extends Parser[A]
  case StringIn(strings: Iterable[String]) extends Parser[String]
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

  /**
   * Parse exactly the given string
   */
  def string(string: String): Parser[String] =
    Exactly(string)

  /**
   * Parse the longest matching string amongst the given strings
   */
  def stringIn(strings: Iterable[String]): Parser[String] =
    StringIn(strings)


  enum Result[A] {
    /** True if this result is a definite failure (an epsilon or committed failure). */
    def isFailure: Boolean =
      this match {
        case Epsilon(_, _) => true
        case Committed(_, _) => true
        case Interrupted(_) => false
        case Parsed(_, _, _) => false
      }

    def isSuccess: Boolean =
      !isFailure

    def map[B](f: A => B): Result[B] =
      this match {
        case Epsilon(i, o) => Epsilon(i, o)
        case Committed(i, o) => Committed(i, o)
        case Interrupted(r) => Interrupted(r.map(f))
        case Parsed(r, i, o) => Parsed(f(r), i, o)
      }

    // Parsed nothing up to one character before the given offset and failed
    case Epsilon(input: String, offset: Int)
    // Parsed up to one character before the given offset and failed
    case Committed(input: String, offset: Int)
    // Successfully parsed all input but was expecting additional input
    case Interrupted[A, B](resumption: Resumption[A, B]) extends Result[B]
    // Successfully parsed input up one character before the given to offset as result
    case Parsed(result: A, input: String, offset: Int)
  }


  final case class Resumption[A,B](
    parser: Parser[A],
    partialResult: A,
    semigroup: Semigroup[A],
    cont: (A, String, Int) => Result[B]
  ) {
    def resume(arg: A, part: String): Result[B] = ???

    def map[C](f: B => C): Resumption[A, C] =
      this.copy(
        cont =
          (a, i, o) =>
            cont(a, i, o) match {
              case Result.Epsilon(i, o) => Result.Epsilon(i, o)
              case Result.Committed(i, o) => Result.Committed(i, o)
              case Result.Interrupted(r) => Result.Interrupted(r.map(f))
              case Result.Parsed(r, i, o) => Result.Parsed(f(r), i, o)
            }
      )

    def withContinuation[C](f: (B, String, Int) => Result[C]): Resumption[A, C] =
      this.copy(
        cont =
          (a, i, o) =>
            cont(a, i, o) match {
              case Result.Epsilon(i, o) => Result.Epsilon(i, o)
              case Result.Committed(i, o) => Result.Committed(i, o)
              case Result.Interrupted(r) => Result.Interrupted(r.withContinuation(f))
              case Result.Parsed(r, i, o) => f(r, i, o)
            }
      )
  }

}
