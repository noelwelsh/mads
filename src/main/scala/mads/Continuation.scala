package mads

import Resumable.{Suspended, Finished}
import Suspendable.Result
import Suspendable.Result.{Epsilon, Committed, Success}

object continuation {
  opaque type Continuation[S, A, B] = Result[A] => Resumable[S, B]
  object Continuation {
    def apply[S, A, B](f: Result[A] => Resumable[S, B]): Continuation[S, A, B] =
      f
  }

  extension [S, A, B](cont: Continuation[S, A, B]) {
    def apply(c: Result[A]): Resumable[S, B] =
      (cont: Result[A] => Resumable[S, B]).apply(c)

    def map[C](f: B => C): Continuation[S, A, C] =
      Continuation((c: Result[A]) => cont(c).map(f))

    def contramap[C](f: C => A): Continuation[S, C, B] =
      Continuation((c: Result[C]) =>
        c match {
          case Success(c, i, s, o) => cont(Success(f(c), i, s, o))
          case Committed(i, s, o)  => cont(Committed(i, s, o))
          case Epsilon(i, o)       => cont(Epsilon(i, o))
        }
      )
  }
}
