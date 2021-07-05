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
  import Suspendable.Result.{Epsilon, Committed, Success}

  def isFinished: Boolean =
    this match {
      case Finished(_)           => true
      case Suspended(_, _, _, _) => false
    }

  def isSuspension: Boolean =
    !isFinished

  def get(using ev: S =:= A): Option[A] =
    this match {
      case Finished(Success(a, _, _, _)) => Some(a)
      case Suspended(_, s, _, cont) =>
        cont(Suspendable.Result.Success(s, "", 0, 0)) match {
          case Finished(Success(a, _, _, _)) => Some(a)
          case Suspended(_, s, _, _) => Some(ev(s))
          case _ => None
        }
      case _ => None
    }

  def getIfFinished: Option[A] =
    this match {
      case Finished(Success(a, _, _, _)) => Some(a)
      // case Finished(Continue(a, _, _))   => Some(a)
      case _ => None
    }

  def map[B](f: A => B): Resumable[S, B] =
    this match {
      case Suspended(p, s, semi, c) => Suspended(p, s, semi, c.map(f))
      case Finished(r) =>
        r match {
          case Epsilon(i, s)      => epsilon(i, s)
          case Committed(i, s, o) => committed(i, s, o)
          // case Continue(a, i, s)   => continue(f(a), i, s)
          case Success(a, i, s, o) => success(f(a), i, s, o)
        }
    }

  def inject(result: S): Resumable[S, A] =
    this match {
      case Suspended(p, s, semi, c) =>
        Suspended(p, semi.combine(s, result), semi, c)
      case Finished(r) => Finished(r)
    }

  /** Resume parsing with the input if this is suspended. */
  def resume(input: String): Resumable[S, A] =
    this match {
      case Suspended(p, r, semi, cont) =>
        p.parse(input, 0) match {
          case Suspended(p2, r2, semi2, c2) =>
            Suspended(p2, semi.combine(r, r2), semi, cont)

          case Finished(result) =>
            result match {
              case Success(r2, i, s, o) =>
                cont(Success(semi.combine(r, r2), i, s, o))

              case other => cont(other)
            }
        }

      case Finished(r) => Finished(r)
    }

  def injectAndResumeOrRestart(
      result: S,
      input: String,
      parser: Suspendable[S, A]
  )(using semigroup: Semigroup[A], ev: S =:= A): Resumable[S, A] =
    this match {
      case Suspended(_, _, _, _) => this.inject(result).resume(input)
      case Finished(r) =>
        r match {
          case Epsilon(i, s)      => epsilon(i, s)
          case Committed(i, s, o) => committed(i, s, o)
          // case Continue(a, i, s)   =>
          //   val r1 = semigroup.combine(a, ev(result))
          //   parser.parse(input).map(r2 => semigroup.combine(r1, r2))
          case Success(a, i, s, o) =>
            val r1 = semigroup.combine(a, ev(result))
            parser.parse(input).map(r2 => semigroup.combine(r1, r2))
        }
    }

  /** Successfully parsed all input and is expecting additional input */
  case Suspended[S, A](
      parser: Suspendable[S, S],
      partialResult: S,
      semigroup: Semigroup[S],
      continuation: Continuation[S, S, A]
  ) extends Resumable[S, A]

  /** Parser has finished with its' input */
  case Finished(result: Suspendable.Result[A])
}
object Resumable {
  import Suspendable.Result.{Epsilon, Committed, Success}

  def epsilon[S, A](input: String, start: Int): Resumable[S, A] =
    Finished(Epsilon(input, start))

  def committed[S, A](input: String, start: Int, offset: Int): Resumable[S, A] =
    Finished(Committed(input, start, offset))

  def success[S, A](
      result: A,
      input: String,
      start: Int,
      offset: Int
  ): Resumable[S, A] =
    Finished(Success(result, input, start, offset))

  def lift[S, A](result: Suspendable.Result[A]): Resumable[S, A] =
    Finished(result)
}
