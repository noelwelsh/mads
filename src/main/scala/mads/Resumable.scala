package mads

import cats.Semigroup
import mads.continuation.*

/** A result type for a parser that may be able to be resumed with additional
  * input
  *
  *   - A is the type of output
  *   - S is the type of suspended output
  */
enum Resumable[S, A] {
  import Resumable.*

  def isFinished: Boolean =
    this match {
      case Finished(_) => true
      case _           => false
    }

  def isSuspension: Boolean =
    !isFinished

  def get: Option[A] =
    this match {
      case Finished(Complete.Success(a, _, _)) => Some(a)
      case _                                   => None
    }

  def map[B](f: A => B): Resumable[S, B] =
    this match {
      case Suspended(p, s, semi, c) => Suspended(p, s, semi, c.map(f))
      case Finished(c) =>
        c match {
          case Complete.Epsilon(i, o)    => epsilon(i, o)
          case Complete.Committed(i, o)  => committed(i, o)
          case Complete.Success(a, i, o) => success(f(a), i, o)
        }
    }

  def inject(result: S): Resumable[S, A] =
    this match {
      case Suspended(p, s, semi, c) =>
        Suspended(p, semi.combine(s, result), semi, c)
      case Finished(c) => Finished(c)
    }

  /** Resume parsing with the input if this is suspended. */
  def resume(input: String): Resumable[S, A] =
    this match {
      case Suspended(p, s, semi, cont) =>
        p.parse(input, 0) match {
          case Suspended(p2, s2, semi2, c2) =>
            Suspended(p2, semi.combine(s, s2), semi, cont)

          case Finished(c) =>
            c match {
              case Complete.Success(s2, i, o) =>
                cont(Complete.Success(semi.combine(s, s2), i, o))

              case other => cont(other)
            }
        }

      case Finished(c) => Finished(c)
    }

  def injectAndResumeOrRestart(
      result: S,
      input: String,
      parser: Suspendable[S, A]
  )(using semigroup: Semigroup[A], ev: S =:= A): Resumable[S, A] =
    get match {
      case Some(a) =>
        val r1 = semigroup.combine(a, ev(result))
        parser.parse(input).map(r2 => semigroup.combine(r1, r2))
      case None => this.inject(result).resume(input)
    }

  /** Successfully parsed all input but was expecting additional input */
  case Suspended[S, A](
      parser: Suspendable[S, S],
      partialResult: S,
      semigroup: Semigroup[S],
      continuation: Continuation[S, A]
  ) extends Resumable[S, A]

  /** Parser has finished with its' input */
  case Finished(complete: Complete[A])
}
object Resumable {
  def epsilon[S, A](input: String, offset: Int): Resumable[S, A] =
    Finished(Complete.Epsilon(input, offset))

  def committed[S, A](input: String, offset: Int): Resumable[S, A] =
    Finished(Complete.Committed(input, offset))

  def success[S, A](result: A, input: String, offset: Int): Resumable[S, A] =
    Finished(Complete.Success(result, input, offset))

  def complete[S, A](complete: Complete[A]): Resumable[S, A] =
    lift(complete)

  def lift[S, A](complete: Complete[A]): Resumable[S, A] =
    Finished(complete)
}
