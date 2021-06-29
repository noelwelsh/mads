package mads

import Resumable.{Suspended, Finished}
import Complete.{Epsilon, Committed, Success}

object continuation {
  opaque type Continuation[S, A] = Complete[S] => Resumable[S, A]
  object Continuation {
    def apply[S, A](f: Complete[S] => Resumable[S, A]): Continuation[S, A] =
      f

    def onSuccess[S, A](f: (S, String, Int) => Resumable[S, A]): Continuation[S, A] =
      (c: Complete[S]) =>
        c match {
          case Success(s, i, o) => f(s, i, o)
          case Committed(i, o) => Resumable.committed(i, o)
          case Epsilon(i, o) => Resumable.epsilon(i, o)
        }
  }

  extension [S, A](cont: Continuation[S, A]) {
    def apply(c: Complete[S]): Resumable[S, A] =
      (cont: Complete[S] => Resumable[S, A]).apply(c)

    def map[B](f: A => B): Continuation[S, B] =
      Continuation((c: Complete[S]) => cont(c).map(f))
  }
}
