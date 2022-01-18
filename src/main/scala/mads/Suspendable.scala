package mads

import cats.Semigroup
import cats.data.NonEmptyChain
import cats.implicits._
import mads.continuation.*
import scala.annotation.nowarn

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

  /** Parse the given input under the assumption that no more input will be
    * available.
    */
  def complete(input: String, offset: Int = 0): Result[A] =
    loop(
      input,
      offset,
      Continuation((ctrl: Control) => (r: Result[A]) => ctrl.lift(r))
    )(Control.complete)

  def parse(input: String, offset: Int = 0): Resumable[S, A] =
    loop(
      input,
      offset,
      Continuation((ctrl: Control) => r => ctrl.lift(r))
    )(Control.suspend)

  def parseToCompletion(
      input: IterableOnce[String],
      offset: Int = 0
  )(using ev: S =:= A): Result[A] = {
    def loop(
        // Invariant: iterator should always have data available
        iterator: Iterator[String],
        result: Resumable[S, A]
    ): Result[A] =
      result match {
        case s @ Suspended(_, result, _, _) =>
          if iterator.hasNext then
            val input = iterator.next()
            if iterator.hasNext then loop(iterator, s.resume(input))
            else s.complete(input)
          else
            throw new IllegalStateException(
              "parseToCompletion invariant did not hold. Input iterator did not have data available."
            )
        case Finished(r) => r
      }

    val it = input.iterator
    if it.hasNext then
      val input = it.next()
      if it.hasNext then loop(it, this.parse(input))
      else this.complete(input)
    else
      throw IllegalStateException(
        "parseToCompleltion was given empty input. Cannot parse from no input."
      )
  }

  /** Parse the input string without suspending or failing in other ways or
    * throw an exception. Mainly useful for tests or quick hacks. def
    * parseOrExn(input: String): A = this.parse(input) match { case
    * Resumable.Finished(Success(a, _, _, _)) => a case other => throw new
    * Exception(s"Parsing failed with $other") }
    */

  def loop[B](
      input: String,
      offset: Int,
      continuation: Continuation[S, A, B]
  )(ctrl: Control): ctrl.F[S, B] = {
    import Suspendable.Result.{Epsilon, Committed, Success}

    this match {

      case Backtrack(source) =>
        source.loop(
          input,
          offset,
          Continuation((ctrl: Control) =>
            (result: Result[A]) =>
              result match {
                case s @ Success(_, _, _, _) => continuation(s)(ctrl)
                case Committed(i, s, _) => continuation(Epsilon(i, s))(ctrl)
                case e @ Epsilon(_, _)  => continuation(e)(ctrl)
              }
          )
        )(ctrl)

      case Map(source, f) =>
        source.loop(
          input,
          offset,
          continuation.contramap(f)
        )(ctrl)

      case OrElse(left, right) =>
        left.loop(
          input,
          offset,
          Continuation((ctrl: Control) =>
            (result: Result[A]) =>
              result match {
                case Epsilon(_, _) =>
                  right.loop(input, offset, continuation)(ctrl)
                case other =>
                  continuation(other)(ctrl)
              }
          )
        )(ctrl)

      case p: Product[s, a, b] =>
        p.left.loop(
          input,
          offset,
          Continuation((ctrl: Control) =>
            (result: Result[a]) =>
              result match {
                case Success(a: a, i, s, o) =>
                  p.right.loop(i, o, continuation.contramap((b: b) => (a, b)))(
                    ctrl
                  )
                case Committed(i, s, o) =>
                  continuation(Committed(i, s, o))(ctrl)
                case Epsilon(i, o) => continuation(Epsilon(i, o))(ctrl)
              }
          )
        )(ctrl)

      case r: Rep[s, a] =>
        def repeat(
            accum: Success[NonEmptyChain[a]],
            ctrl: Control
        ): ctrl.F[S, B] =
          // Don't allow infinite loops on parsers that don't make progress
          if accum.offset == accum.input.size then continuation(accum)(ctrl)
          else
            r.source.loop(
              accum.input,
              accum.offset,
              Continuation((ctrl: Control) =>
                (result: Result[a]) =>
                  result match {
                    case Success(a, i, s, o) =>
                      repeat(Success(accum.result :+ a, i, offset, o), ctrl)
                    case Committed(i, s, o) => continuation(accum)(ctrl)
                    case Epsilon(i, o)      => continuation(accum)(ctrl)
                  }
              )
            )(ctrl)

        r.source.loop(
          input,
          offset,
          Continuation((ctrl: Control) =>
            (result: Result[a]) =>
              result match {
                case Success(a, i, s, o) =>
                  repeat(Success(NonEmptyChain(a), i, s, o), ctrl)
                case Committed(i, s, o) =>
                  continuation(Committed(i, s, o))(ctrl)
                case Epsilon(i, o) => continuation(Epsilon(i, o))(ctrl)
              }
          )
        )(ctrl)

      case Advance(parser) =>
        ctrl.advance(input, offset, parser, continuation)

      case Commit(parser) =>
        ctrl.commit(input, offset, parser, continuation)

      case Resume(parser, lift, semigroup) =>
        ctrl.resume(input, offset, this, parser, lift, semigroup, continuation)

      // case _ =>
      //   throw new IllegalStateException("This case should never happen. It's only here to stop exhaustivity checking from complaining.")
    }
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

  /** Lift a Parser into a Supspendable parser that will continue to next parser
    * when input ends
    */
  case Advance[S, A](parser: Parser[A]) extends Suspendable[S, A]

  /** Lift a Parser into a Suspendable parser without allowing for resumption */
  case Commit[S, A](parser: Parser[A]) extends Suspendable[S, A]

  /** Lift a Parser into a Suspendable parser allowing for resumption */
  case Resume[A](parser: Parser[A], lift: String => A, semigroup: Semigroup[A])
      extends Suspendable[A, A]
}
object Suspendable {
  def fromParser[S, A](parser: Parser[A]): Suspendable[S, A] =
    Suspendable.Commit(parser)

  enum Result[+A] {

    def unsafeGet: A =
      this match {
        case Epsilon(_, _) =>
          throw new IllegalStateException(
            s"This result was an epsilon failure, not a success: $this"
          )
        case Committed(_, _, _) =>
          throw new IllegalStateException(
            s"This result was committed failure, not a success: $this"
          )
        case Success(a, _, _, _) => a
      }

    def map[B](f: A => B): Result[B] =
      this match {
        case Epsilon(i, o)       => Epsilon(i, o)
        case Committed(i, s, o)  => Committed(i, s, o)
        case Success(a, i, s, o) => Success(f(a), i, s, o)
      }

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
