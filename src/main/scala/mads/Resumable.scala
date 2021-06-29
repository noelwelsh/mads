package mads

import cats.Semigroup

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

  def map[B](f: A => B): Resumable[S, B] =
    this match {
      case Suspended(s) => Suspended(s.map(f))
      case Finished(c) =>
        c match {
          case Complete.Epsilon(i, o)    => epsilon(i, o)
          case Complete.Committed(i, o)  => committed(i, o)
          case Complete.Success(a, i, o) => success(f(a), i, o)
        }
    }

  /** Successfully parsed all input but was expecting additional input */
  case Suspended[S, A](suspension: Suspension[S, A]) extends Resumable[S, A]

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

  def lift[S, A](complete: Complete[A]): Resumable[S, A] =
    Finished(complete)
}
