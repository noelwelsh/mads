package mads

import cats.Semigroup
import cats.data.NonEmptyChain
import cats.implicits._
import mads.continuation.*

/** A parser that can be suspended
  *
  *   - S is the type of suspended values
  *   - A is the type of output
  */
enum Suspendable[S, A] {
  import Resumable.{Suspended, Finished}
  import Suspendable.Result
  import Suspendable.Result.{Epsilon, Committed, Success}

  /** Apply two parsers in sequence. Both must be resumable with the same type,
    * as from the outside we cannot tell whether we are resuming the left or
    * right side.
    */
  def ~[B](that: Suspendable[S, B]): Suspendable[S, (A, B)] =
    Product(this, that)

  def *>[B](that: Suspendable[S, B]): Suspendable[S, B] =
    (this.void ~ that).map(_.apply(1))

  def <*[B](that: Suspendable[S, B]): Suspendable[S, A] =
    (this ~ that.void).map(_.apply(0))

  /** If this parser fails with a committed result convert it to an epsilon
    * result so that processing can continue with another parser. Usually used
    * in conjection with orElse.
    */
  def backtrack: Suspendable[S, A] =
    Backtrack(this)

  def orElse(that: Suspendable[S, A]): Suspendable[S, A] =
    OrElse(this, that)

  def map[B](f: A => B): Suspendable[S, B] =
    Map(this, f)

  /** Repeat this parser one or more times */
  def rep: Suspendable[S, NonEmptyChain[A]] =
    Rep(this)

  /** Ignore the output of this parser */
  def void: Suspendable[S, Unit] =
    this.map(_ => ())

  def loop[B](
      input: String,
      offset: Int,
      continuation: Continuation[S, A, B]
  ): Resumable[S, B] = {
    import Suspendable.Result.{Epsilon, Committed, Success}

    this match {

      case Backtrack(source) =>
        source.loop(
          input,
          offset,
          Continuation((result) =>
            result match {
              case s @ Success(_, _, _, _) => continuation(s)
              case Committed(i, s, _)      => continuation(Epsilon(i, s))
              case e @ Epsilon(_, _)       => continuation(e)
            }
          )
        )

      case Map(source, f) =>
        source.loop(
          input,
          offset,
          continuation.contramap(f)
        )

      case OrElse(left, right) =>
        left.loop(
          input,
          offset,
          Continuation(c =>
            c match {
              case Epsilon(_, _) =>
                right.loop(input, offset, continuation)
              case other =>
                continuation(other)
            }
          )
        )

      case p: Product[s, a, b] =>
        p.left.loop(
          input,
          offset,
          Continuation((result: Result[a]) =>
            result match {
              case Success(a: a, i, s, o) =>
                p.right.loop(i, o, continuation.contramap((b: b) => (a, b)))
              case Committed(i, s, o) => continuation(Committed(i, s, o))
              case Epsilon(i, o)      => continuation(Epsilon(i, o))
            }
          )
        )

      case r: Rep[s, a] =>
        def repeat(accum: Success[NonEmptyChain[a]]): Resumable[S, B] =
          // Don't allow infinite loops on parsers that don't make progress
          if accum.offset == accum.input.size then continuation(accum)
          else
            r.source.loop(
              accum.input,
              accum.offset,
              Continuation((result: Result[a]) =>
                result match {
                  case Success(a, i, s, o) =>
                    repeat(Success(accum.result :+ a, i, offset, o))
                  case Committed(i, s, o) => continuation(accum)
                  case Epsilon(i, o)      => continuation(accum)
                }
              )
            )

        r.source.loop(
          input,
          offset,
          Continuation((result: Result[a]) =>
            result match {
              case Success(a, i, s, o) =>
                repeat(Success(NonEmptyChain(a), i, s, o))
              case Committed(i, s, o) => continuation(Committed(i, s, o))
              case Epsilon(i, o)      => continuation(Epsilon(i, o))
            }
          )
        )

      case Advance(parser) =>
        parser.parse(input, offset) match {
          case Parser.Result.Success(a, i, s, o) =>
            continuation(Success(a, i, s, o))
          // Advance converts Continue to Success and we move on to the next
          // parser, which will usually Epsilon complete and be suspended.
          case Parser.Result.Continue(a, i, s) =>
            continuation(Success(a, i, s, i.size))
          case Parser.Result.Committed(i, s, o) =>
            continuation(Committed(i, s, o))
          case Parser.Result.Epsilon(i, o) => continuation(Epsilon(i, o))
        }

      case Resume(parser, lift, semigroup) =>
        parser.parse(input, offset) match {
          case Parser.Result.Success(a, i, s, o) =>
            continuation(Success(a, i, s, o))
          case Parser.Result.Continue(a, i, s) =>
            Resumable.Suspended(this, a, semigroup, continuation)
          case Parser.Result.Committed(i, s, o) =>
            continuation(Committed(i, s, o))
          case Parser.Result.Epsilon(i, o) =>
            continuation(Epsilon(i, o))
        }

      case Commit(parser) =>
        parser.parse(input, offset) match {
          case Parser.Result.Success(a, i, s, o) =>
            continuation(Success(a, i, s, o))
          // This parser is unsuspendable so we convert a continue into committed
          case Parser.Result.Continue(a, i, s) =>
            continuation(Committed(i, s, i.size))
          case Parser.Result.Committed(i, s, o) =>
            continuation(Committed(i, s, o))
          case Parser.Result.Epsilon(i, o) => continuation(Epsilon(i, o))
        }

      case _ =>
        throw new IllegalStateException("This case should never happen. It's only here to stop exhaustivity checking from complaining.")
    }
  }

  def parse(input: String, offset: Int = 0): Resumable[S, A] =
    loop(
      input,
      offset,
      Continuation(c => Resumable.lift(c))
    )

  def parseToCompletion(
      input: IterableOnce[String],
      offset: Int = 0
  )(using ev: S =:= A): Result[A] = {
    def loop(
        previousInput: String,
        input: Iterator[String],
        result: Resumable[S, A]
    ): Result[A] =
      result match {
        case s @ Suspended(_, result, _, _) =>
          if input.hasNext then
            val nextInput = input.next()
            loop(nextInput, input, s.resume(nextInput))
          else Success(ev(result), previousInput, 0, previousInput.size)
        case Finished(r) => r
      }

    val it = input.iterator
    if it.hasNext then
      val nextInput = it.next()
      loop(nextInput, it, this.parse(nextInput))
    else throw IllegalStateException("Cannot parse from no input.")
  }

  /** Parse the input string without suspending or failing in other ways or
    * throw an exception. Mainly useful for tests or quick hacks.
    */
  def parseOrExn(input: String): A =
    this.parse(input) match {
      case Resumable.Finished(Success(a, _, _, _)) => a
      case other => throw new Exception(s"Parsing failed with $other")
    }

  case Backtrack[S, A](parser: Suspendable[S, A]) extends Suspendable[S, A]
  case Map[S, A, B](source: Suspendable[S, A], f: A => B)
      extends Suspendable[S, B]
  case OrElse[S, A](left: Suspendable[S, A], right: Suspendable[S, A])
      extends Suspendable[S, A]
  case Product[S, A, B](left: Suspendable[S, A], right: Suspendable[S, B])
      extends Suspendable[S, (A, B)]
  case Rep[S, A](source: Suspendable[S, A])
      extends Suspendable[S, NonEmptyChain[A]]

  /** Lift a Parser into a Suspendable parser allowing for resumption */
  case Resume[A](parser: Parser[A], lift: String => A, semigroup: Semigroup[A])
      extends Suspendable[A, A]

  /** Lift a Parser into a Supspendable parser that will continue to next parser
    * when input ends
    */
  case Advance[S, A](parser: Parser[A]) extends Suspendable[S, A]

  /** Lift a Parser into a Suspendable parser without allowing for resumption */
  case Commit[S, A](parser: Parser[A]) extends Suspendable[S, A]
}
object Suspendable {
  def fromParser[S, A](parser: Parser[A]): Suspendable[S, A] =
    Suspendable.Commit(parser)

  enum Result[+A] {

    /** Parsed nothing starting at the given position in the input */
    case Epsilon(input: String, start: Int)

    /** Parsed up to and including one character before the given offset and
      * failed
      */
    case Committed(input: String, start: Int, offset: Int)

    /** Successfully parsed input up to and including one character before the
      * given offset as result
      */
    case Success(result: A, input: String, start: Int, offset: Int)
  }
}
