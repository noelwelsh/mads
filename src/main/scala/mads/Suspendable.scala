package mads

import cats.Semigroup

/** A parser that can be suspended
  *
  *   - S is the type of suspended values
  *   - A is the type of output
  */
enum Suspendable[S, A] {

  /** Apply two parsers in sequence. Both must be resumable with the same type,
    * as from the outside we cannot tell whether we are resuming the left or
    * right side.
    */
  def ~[B](that: Suspendable[S, B]): Suspendable[S, (A, B)] =
    Product(this, that)

  def map[B](f: A => B): Suspendable[S, B] =
    Map(this, f)

  def void: Suspendable[S, Unit] =
    this.map(_ => ())

  def loop[B](
      input: String,
      offset: Int,
      continuation: (A, String, Int) => Resumable[S, B]
  ): Resumable[S, B] = {
    import Complete.{Epsilon, Committed, Success}

    this match {
      case Product(left, right) =>
        left.loop(
          input,
          offset,
          (a, i, o) =>
            right.loop(i, o, (b, i2, o2) => continuation((a, b), i2, o2))
        )

      case Map(source, f) =>
        source.loop(input, offset, (a, i, o) => continuation(f(a), i, o))

      case Suspend(parser, lift, semigroup) =>
        parser.parse(input, offset) match {
          case Success(a, i, o) => continuation(a, i, o)
          case Committed(i, o) =>
            if offset == i.size then
              val suspension =
                Suspension(this, lift(i), semigroup, continuation)
              Resumable.Suspended(suspension)
            else Resumable.committed(i, o)
          case Epsilon(i, o) => Resumable.epsilon(i, o)
        }

      case Unsuspendable(parser) =>
        parser.parse(input, offset) match {
          case Success(a, i, o) => continuation(a, i, o)
          case Committed(i, o)  => Resumable.committed(i, o)
          case Epsilon(i, o)    => Resumable.epsilon(i, o)
        }
    }
  }

  def parse(input: String, offset: Int = 0): Resumable[S, A] = {
    loop(input, offset, (a, i, o) => Resumable.success(a, i, o))
  }

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
