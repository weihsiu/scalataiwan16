package scalataiwan16

import aiyou._
import cats._
import org.atnos.eff._

object IOEffect {
  type _io[R] = IO |= R
  def fromIO[R : _io, A](action: IO[A]): Eff[R, A] = Eff.send[IO, R, A](action)
  implicit class IOOps[R, A](effect: Eff[R, A]) {
    def runIO[U](implicit m: Member.Aux[IO, R, U]): Eff[U, A] = {
      Interpret.translateNat(effect)(new (IO ~> Eff[U, ?]) {
        def apply[B](io: IO[B]): Eff[U, B] = Eff.pure(io.unsafePerformIO)
      })
    }
  }
}
