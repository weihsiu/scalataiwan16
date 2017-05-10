package scalataiwan16

import cats._
import cats.data._
import cats.data.WriterT._
import cats.implicits._
import scala.language.higherKinds

object MTLSandbox {
  def main(args: Array[String]): Unit = {
    def program[F[_]](implicit MR: MonadReader[F, String], MW: MonadWriter[F, String]): F[Int] =
      MR.flatMap(MR.ask) { c =>
        MW.map(MW.tell(c + "!")) { _ =>
          123
        }
      }

    def program1[F[_]](implicit MR: MonadReader[F, String]): F[String] =
      MR.map(MR.ask)(c => c)

    type RW[A] = Kleisli[Writer[String, ?], String, A]

    implicit def kleisliMonadWriter[F[_], W, R](implicit MW: MonadWriter[F, W]): MonadWriter[Kleisli[F, R, ?], W] = new MonadWriter[Kleisli[F, R, ?], W] {
      val F = Monad[F]
      def flatMap[A, B](fa: Kleisli[F, R, A])(f: A => Kleisli[F, R, B]): Kleisli[F, R, B] = fa.flatMap(f)
      def listen[A](fa: Kleisli[F, R, A]): Kleisli[F, R, (W, A)] = fa.flatMap(x => Kleisli.lift(MW.listen(F.pure(x))))
      def pass[A](fa: Kleisli[F, R, (W => W, A)]): Kleisli[F, R, A] = fa.flatMap(x => Kleisli.lift(MW.pass(F.pure(x))))
      def pure[A](x: A): Kleisli[F, R, A] = Kleisli.pure(x)
      def tailRecM[A, B](a: A)(f: A => Kleisli[F, R, Either[A, B]]): Kleisli[F, R, B] = ???
      def writer[A](aw: (W, A)): Kleisli[F, R, A] = Kleisli.lift(MW.writer(aw))
    }
    implicit val mwt: MonadWriter[WriterT[Id, String, ?], String] = catsDataMonadWriterForWriterT[Id, String]
    val mw: MonadWriter[Kleisli[Writer[String, ?], String, ?], String] = kleisliMonadWriter[Writer[String, ?], String, String] //MonadWriter[RW, String]
    println(program[RW](MonadReader[RW, String], mw).run("hello").run)
  }
}
