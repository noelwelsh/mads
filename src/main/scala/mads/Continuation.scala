package mads

import Resumable.{Suspended, Finished}
import Parser.Result
import Parser.Result.{Epsilon, Committed, Continue, Success}

object continuation {
  opaque type Continuation[S, A] = Result[S] => Resumable[S, A]
  object Continuation {
    def apply[S, A](f: Result[S] => Resumable[S, A]): Continuation[S, A] =
      f

    def onSuccess[S, A](f: (S, String, Int) => Resumable[S, A]): Continuation[S, A] =
      (c: Result[S]) =>
        c match {
          case Success(r, i, s, o) => f(r, i, o)
          case Continue(r, i, s) => Resumable.continue(r, i, s)
          case Committed(i, s, o) => Resumable.committed(i, s, o)
          case Epsilon(i, s) => Resumable.epsilon(i, s)
        }
  }

  extension [S, A](cont: Continuation[S, A]) {
    def apply(c: Result[S]): Resumable[S, A] =
      (cont: Result[S] => Resumable[S, A]).apply(c)

    def map[B](f: A => B): Continuation[S, B] =
      Continuation((c: Result[S]) => cont(c).map(f))
  }
}
