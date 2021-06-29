package mads

import Resumable.{Suspended, Finished}
import Complete.{Epsilon, Committed, Success}

object continuation {
  opaque type Continuation[S, A, B] = Complete[A] => Resumable[S, B]
  object Continuation {
    def apply[S, A, B](
        f: Complete[A] => Resumable[S, B]
    ): Continuation[S, A, B] =
      f

    def onSuccess[S, A, B](
        f: (A, String, Int) => Resumable[S, B]
    ): Continuation[S, A, B] =
      (c: Complete[A]) =>
        c match {
          case Success(a, i, o) => f(a, i, o)
          case Committed(i, o)  => Resumable.committed(i, o)
          case Epsilon(i, o)    => Resumable.epsilon(i, o)
        }
  }

  extension [S, A, B](cont: Continuation[S, A, B]) {
    def apply(c: Complete[A]): Resumable[S, B] =
      (cont: Complete[A] => Resumable[S, B]).apply(c)

    def map[C](f: B => C): Continuation[S, A, C] =
      Continuation((c: Complete[A]) => cont(c).map(f))
  }
}
