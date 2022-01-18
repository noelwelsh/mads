package mads

import cats.Semigroup
import mads.continuation.*

/** Controls how parsers are turned into suspendable. Seeking a better name. */
trait Control {
  type F[_, _]
  val aux: Control.Aux[F]

  def advance[S, A, B](
      input: String,
      offset: Int,
      parser: Parser[A],
      cont: Continuation[S, A, B]
  ): F[S, B]
  def commit[S, A, B](
      input: String,
      offset: Int,
      parser: Parser[A],
      cont: Continuation[S, A, B]
  ): F[S, B]
  def resume[S, A](
      input: String,
      offset: Int,
      source: Suspendable[S, S],
      parser: Parser[S],
      lift: String => S,
      semigroup: Semigroup[S],
      cont: Continuation[S, S, A]
  ): F[S, A]
  def map[S, A, B](fa: F[S, A])(f: A => B): F[S, B]
  def lift[S, A](result: Suspendable.Result[A]): F[S, A]
}
object Control {
  import Suspendable.Result.{Success, Committed, Epsilon}

  type Aux[FF[_, _]] = Control { type F[S, A] = FF[S, A] }

  object suspend extends Control {
    type F[S, A] = Resumable[S, A]
    type Aux = Control.Aux[Resumable]
    val aux: Control.Aux[Resumable] = this

    def advance[S, A, B](
        input: String,
        offset: Int,
        parser: Parser[A],
        cont: Continuation[S, A, B]
    ): Resumable[S, B] =
      parser.parse(input, offset) match {
        case Parser.Result.Success(a, i, s, o) =>
          cont(Success(a, i, s, o))(this: Aux)
        case Parser.Result.Continue(a, i, s) =>
          cont(Success(a, i, s, i.size))(this: Aux)
        case Parser.Result.Committed(i, s, o) =>
          cont(Committed(i, s, o))(this: Aux)
        case Parser.Result.Epsilon(i, o) => cont(Epsilon(i, o))(this: Aux)
      }

    def commit[S, A, B](
        input: String,
        offset: Int,
        parser: Parser[A],
        cont: Continuation[S, A, B]
    ): Resumable[S, B] =
      parser.parse(input, offset) match {
        case Parser.Result.Success(a, i, s, o) =>
          cont(Success(a, i, s, o))(this: Aux)
        case Parser.Result.Continue(a, i, s) =>
          cont(Committed(i, s, i.size))(this: Aux)
        case Parser.Result.Committed(i, s, o) =>
          cont(Committed(i, s, o))(this: Aux)
        case Parser.Result.Epsilon(i, o) => cont(Epsilon(i, o))(this: Aux)
      }

    def resume[S, A](
        input: String,
        offset: Int,
        source: Suspendable[S, S],
        parser: Parser[S],
        lift: String => S,
        semigroup: Semigroup[S],
        cont: Continuation[S, S, A]
    ): Resumable[S, A] =
      parser.parse(input, offset) match {
        case Parser.Result.Success(a, i, s, o) =>
          cont(Success(a, i, s, o))(this: Aux)
        case Parser.Result.Continue(a, i, s) =>
          Resumable.Suspended(source, a, semigroup, cont)
        case Parser.Result.Committed(i, s, o) =>
          cont(Committed(i, s, o))(this: Aux)
        case Parser.Result.Epsilon(i, o) => cont(Epsilon(i, o))(this: Aux)
      }

    def map[S, A, B](fa: Resumable[S, A])(f: A => B): Resumable[S, B] =
      fa.map(f)

    def lift[S, A](result: Suspendable.Result[A]): Resumable[S, A] =
      Resumable.Finished(result)
  }

  type Complete[S, A] = Suspendable.Result[A]
  object complete extends Control {
    type F[S, A] = Complete[S, A]
    type Aux = Control.Aux[Complete]
    val aux: Control.Aux[Complete] = this

    def advance[S, A, B](
        input: String,
        offset: Int,
        parser: Parser[A],
        cont: Continuation[S, A, B]
    ): Complete[S, B] =
      parser.parse(input, offset) match {
        case Parser.Result.Success(a, i, s, o) =>
          cont(Success(a, i, s, o))(this: Aux)
        case Parser.Result.Continue(a, i, s) =>
          cont(Success(a, i, s, i.size))(this: Aux)
        case Parser.Result.Committed(i, s, o) =>
          cont(Committed(i, s, o))(this: Aux)
        case Parser.Result.Epsilon(i, o) => cont(Epsilon(i, o))(this: Aux)
      }

    def commit[S, A, B](
        input: String,
        offset: Int,
        parser: Parser[A],
        cont: Continuation[S, A, B]
    ): Complete[S, B] =
      parser.parse(input, offset) match {
        case Parser.Result.Success(a, i, s, o) =>
          cont(Success(a, i, s, o))(this: Aux)
        case Parser.Result.Continue(a, i, s) =>
          cont(Success(a, i, s, i.size))(this: Aux)
        case Parser.Result.Committed(i, s, o) =>
          cont(Committed(i, s, o))(this: Aux)
        case Parser.Result.Epsilon(i, o) => cont(Epsilon(i, o))(this: Aux)
      }

    def resume[S, A](
        input: String,
        offset: Int,
        source: Suspendable[S, S],
        parser: Parser[S],
        lift: String => S,
        semigroup: Semigroup[S],
        cont: Continuation[S, S, A]
    ): Complete[S, A] =
      parser.parse(input, offset) match {
        case Parser.Result.Success(a, i, s, o) =>
          cont(Success(a, i, s, o))(this: Aux)
        case Parser.Result.Continue(a, i, s) =>
          cont(Success(a, i, s, i.size))(this: Aux)
        case Parser.Result.Committed(i, s, o) =>
          cont(Committed(i, s, o))(this: Aux)
        case Parser.Result.Epsilon(i, o) => cont(Epsilon(i, o))(this: Aux)
      }

    def map[S, A, B](fa: Complete[S, A])(f: A => B): Complete[S, B] =
      fa match {
        case Success(a, i, s, o) => Success(f(a), i, s, o)
        case Committed(i, s, o)  => Committed(i, s, o)
        case Epsilon(i, s)       => Epsilon(i, s)
      }

    def lift[S, A](result: Suspendable.Result[A]): Complete[S, A] =
      result
  }
}
