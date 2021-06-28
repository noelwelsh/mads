package mads

import cats.Semigroup

/** Holds data for resuming a parser:
  *   - the parser to resume;
  *   - the partially parsed input, which is not reparsed;
  *   - a semigroup to combine injected parsed input with the partial result;
  *     and
  *   - a continuation that accepts the parsed result and new input and
  *     continues parsing.
  */
final case class Suspension[S, A](
    parser: Suspendable[S, S],
    partialResult: S,
    semigroup: Semigroup[S],
    continuation: (S, String, Int) => Resumable[S, A]
) {
  import Resumable.{Suspended, Finished}
  import Complete.{Epsilon, Committed, Success}

  def map[B](f: A => B): Suspension[S, B] =
    this.copy(continuation = (s, i, o) => continuation(s, i, o).map(f))

  /** Add already parsed input to this suspension
    */
  def inject(result: S): Suspension[S, A] =
    this.copy(
      partialResult = semigroup.combine(partialResult, result)
    )

  /** Parse additional input.
    */
  def parse(input: String): Resumable[S, A] =
    parser.parse(input, 0) match {
      case Suspended(s) =>
        Suspended(
          this.copy(partialResult =
            semigroup.combine(partialResult, s.partialResult)
          )
        )
      case Finished(c) =>
        c match {
          case Complete.Epsilon(i, o)   => Resumable.epsilon(i, o)
          case Complete.Committed(i, o) => Resumable.committed(i, o)
          case Complete.Success(s, i, o) =>
            continuation(semigroup.combine(partialResult, s), i, o)
        }
    }
}
