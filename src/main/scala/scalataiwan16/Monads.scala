package scalataiwan16

import cats._
import cats.data._
import cats.implicits._
import scala.language.higherKinds

case class ReaderOption[R, A](value: Reader[R, Option[A]])

class ReaderOptionMonad[R] extends Monad[ReaderOption[R, ?]] {
  implicit val F: Monad[Reader[R, ?]] = implicitly[Monad[Reader[R, ?]]]
  def pure[A](x: A): ReaderOption[R, A] = ReaderOption(x.some.pure[Reader[R, ?]])
  def flatMap[A, B](fa: ReaderOption[R, A])(f: (A) => ReaderOption[R, B]): ReaderOption[R, B] =
    ReaderOption(fa.value.flatMap({
      case Some(x) => f(x).value
      case None => none.pure[Reader[R, ?]]
    }))
  def tailRecM[A, B](a: A)(f: A => ReaderOption[R, Either[A, B]]): ReaderOption[R, B] =
    ReaderOption(
      F.tailRecM(a)(a0 =>
        F.map(f(a0).value)(_.fold(Either.right[A, Option[B]](None))(_.map(b => Some(b): Option[B])))))
}

case class WriterOption[W, A](value: Writer[W, Option[A]])

class WriterOptionMonad[W : Semigroup] extends Monad[WriterOption[W, ?]] {
  implicit val F: Monad[Writer[W, ?]] = implicitly[Monad[Writer[W, ?]]]
  def pure[A](x: A): WriterOption[W, A] = WriterOption(x.some.pure[Writer[W, ?]])
  def flatMap[A, B](fa: WriterOption[W, A])(f: (A) => WriterOption[W, B]): WriterOption[W, B] =
    WriterOption(fa.value.flatMap({
      case Some(x) => f(x).value
      case None => none.pure[Writer[W, ?]]
    }))
  def tailRecM[A, B](a: A)(f: A => WriterOption[W, Either[A, B]]): WriterOption[W, B] =
    WriterOption(
      F.tailRecM(a)(a0 =>
        F.map(f(a0).value)(_.fold(Either.right[A, Option[B]](None))(_.map(b => Some(b): Option[B])))))
}

case class GenericOption[F[_], A](value: F[Option[A]])

class GenericOptionMonad[F[_]] extends Monad[GenericOption[F, ?]] {
  implicit val F: Monad[F] = implicitly[Monad[F]]
  def pure[A](x: A): GenericOption[F, A] = GenericOption(x.some.pure[F])
  def flatMap[A, B](fa: GenericOption[F, A])(f: (A) => GenericOption[F, B]): GenericOption[F, B] =
    GenericOption(fa.value.flatMap({
      case Some(x) => f(x).value
      case None => none.pure[F]
    }))
  def tailRecM[A, B](a: A)(f: A => GenericOption[F, Either[A, B]]): GenericOption[F, B] =
    GenericOption(
      F.tailRecM(a)(a0 =>
        F.map(f(a0).value)(_.fold(Either.right[A, Option[B]](None))(_.map(b => Some(b): Option[B])))))
}