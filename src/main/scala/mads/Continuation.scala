package mads

import cats.{Functor, Contravariant}
import Resumable.{Suspended, Finished}
import Suspendable.Result
import Suspendable.Result.{Epsilon, Committed, Success}

object continuation {
  opaque type Continuation[S, A, B] =
    (ctrl: Control) => Result[A] => ctrl.F[S, B]
  object Continuation {
    def apply[S, A, B](
        f: (ctrl: Control) => Result[A] => ctrl.F[S, B]
    ): Continuation[S, A, B] =
      f
  }

  extension [S, A, B](cont: Continuation[S, A, B]) {
    def apply[F[_, _]](c: Result[A])(ctrl: Control.Aux[F]): F[S, B] =
      cont.apply(ctrl).apply(c)

    def map[C](f: B => C): Continuation[S, A, C] =
      Continuation((ctrl: Control) =>
        (r: Result[A]) => ctrl.map(cont(ctrl)(r))(f)
      )

    def contramap[C](f: C => A): Continuation[S, C, B] =
      Continuation((ctrl: Control) =>
        (r: Result[C]) =>
          r match {
            case Success(c, i, s, o) => cont(ctrl)(Success(f(c), i, s, o))
            case Committed(i, s, o)  => cont(ctrl)(Committed(i, s, o))
            case Epsilon(i, o)       => cont(ctrl)(Epsilon(i, o))
          }
      )
  }
}
