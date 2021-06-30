package mads

import cats.Semigroup
import mads.continuation.*

/** A parser that can be suspended
  *
  *   - S is the type of suspended values
  *   - A is the type of output
  */
enum Suspendable[S, A] {
  import Resumable.{Suspended, Finished}
  import Parser.Result
  import Parser.Result.{Epsilon, Committed, Continue, Success}

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

  def orElse(that: Suspendable[S, A]): Suspendable[S, A] =
    OrElse(this, that)

  def map[B](f: A => B): Suspendable[S, B] =
    Map(this, f)

  def void: Suspendable[S, Unit] =
    this.map(_ => ())

  def loop[B](
      input: String,
      offset: Int,
      continuation: Continuation[S, A, B]
  ): Resumable[S, B] = {
    import Parser.Result.{Epsilon, Committed, Continue, Success}

    this match {
      case Map(source, f) =>
        source.loop(
          input,
          offset,
          Continuation.onSuccess((a, i, s, o) =>
            continuation(Success(f(a), i, s, o))
          )
        )

      case OrElse(left, right) =>
        left.loop(
          input,
          offset,
          Continuation(c =>
            c match {
              case e: Epsilon[A] => right.loop(input, offset, continuation)
              case other         => continuation(other)
            }
          )
        )

      case Product(left, right) =>
        left.loop(
          input,
          offset,
          Continuation.onSuccess((a, i, s, o) =>
            right.loop(
              i,
              o,
              Continuation.onSuccess((b, i2, s2, o2) =>
                continuation(Success((a, b), i2, s, o2))
              )
            )
          )
        )

      case Suspend(parser, lift, semigroup) =>
        parser.parse(input, offset) match {
          case Success(a, i, s, o) => continuation(Success(a, i, s, o))
          case Continue(a, i, s) => Resumable.Suspended(this, a, semigroup, continuation)
          case Committed(i, s, o) => Resumable.committed(i, s, o)
          case Epsilon(i, o) =>
            if o == i.size then
              Resumable.Suspended(
                this,
                lift(""),
                semigroup,
                continuation
              )
            else Resumable.epsilon(i, o)
        }

      case Unsuspendable(parser) =>
        parser.parse(input, offset) match {
          case s: Success[A] => continuation(s)
          // This parser is unsuspendable so we convert a continue into committed
          case Continue(a, i, s) => Resumable.committed(i, s, i.size)
          case Committed(i, s, o)  => Resumable.committed(i, s, o)
          case Epsilon(i, s)    => Resumable.epsilon(i, s)
        }
    }
  }

  def parse(input: String, offset: Int = 0): Resumable[S, A] =
    loop(
      input,
      offset,
      Continuation.onSuccess((a, i, s, o) => Resumable.success(a, i, s, o))
    )

  def parseToCompletion(
      input: IterableOnce[String],
      offset: Int = 0
  ): Result[A] = {
    def loop(
        previousInput: String,
        input: Iterator[String],
        result: Resumable[S, A]
    ): Result[A] =
      result match {
        case s @ Suspended(_, _, _, _) =>
          if input.hasNext then
            val nextInput = input.next()
            loop(nextInput, input, s.resume(nextInput))
          else Committed(previousInput, 0, previousInput.size)
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

  case OrElse[S, A](left: Suspendable[S, A], right: Suspendable[S, A])
      extends Suspendable[S, A]
  case Product[S, A, B](left: Suspendable[S, A], right: Suspendable[S, B])
      extends Suspendable[S, (A, B)]
  case Map[S, A, B](source: Suspendable[S, A], f: A => B)
      extends Suspendable[S, B]

  /** Lift a Parser into a Suspendable parser allowing for resumption */
  case Suspend[A](parser: Parser[A], lift: String => A, semigroup: Semigroup[A])
      extends Suspendable[A, A]

  /** Lift a Parser into a Suspendable parser without allowing for resumption */
  case Unsuspendable[S, A](parser: Parser[A]) extends Suspendable[S, A]
}
object Suspendable {
  def fromParser[S, A](parser: Parser[A]): Suspendable[S, A] =
    Suspendable.Unsuspendable(parser)
}
