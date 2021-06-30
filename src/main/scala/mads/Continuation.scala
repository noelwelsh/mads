package mads

import Resumable.{Suspended, Finished}
import Parser.Result
import Parser.Result.{Epsilon, Committed, Continue, Success}

object continuation {
  opaque type Continuation[S, A, B] = Result[A] => Resumable[S, B]
  object Continuation {
    def apply[S, A, B](f: Result[A] => Resumable[S, B]): Continuation[S, A, B] =
      f

    def onSuccess[S, A, B](f: (A, String, Int, Int) => Resumable[S, B]): Continuation[S, A, B] =
      (c: Result[A]) =>
        c match {
          case Success(r, i, s, o) => f(r, i, s, o)
          case Continue(r, i, s) => f(r, i, s, i.size)
          case Committed(i, s, o) => Resumable.committed(i, s, o)
          case Epsilon(i, s) => Resumable.epsilon(i, s)
        }
  }

  extension [S, A, B](cont: Continuation[S, A, B]) {
    def apply(c: Result[A]): Resumable[S, B] =
      (cont: Result[A] => Resumable[S, B]).apply(c)

    def map[C](f: B => C): Continuation[S, A, C] =
      Continuation((c: Result[A]) => cont(c).map(f))
  }
}
