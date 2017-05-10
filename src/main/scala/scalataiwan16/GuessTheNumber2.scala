package scalataiwan16

import scala.io.StdIn
import scala.language.higherKinds

import aiyou._
import aiyou.implicits._
import cats._
import cats.data._
import cats.implicits._

object GuessTheNumber2 extends App {
  case class Config(min: Int, max: Int)
  case class Context(num: Option[Int])
  type GTN[F[_], A] = StateT[WriterT[ReaderT[F, Config, ?], Vector[String], ?], Context, A]
//  def guess[F[_] : Monad](n: Int)(implicit MR: MonadReader[F, Config], MW: MonadWriter[F, Vector[String]], MS: MonadState[F, Context]): F[String] = for {
//    _ <- MW.tell(Vector(s"n = $n"))
//    config <- MR.ask
//    Config(min, max) = config
//    _ <- MW.tell(Vector(s"min = $min, max = $max"))
//    context <- MS.get
//    Context(Some(num)) = context
//    _ <- MW.tell(Vector(s"num = $num"))
//  } yield
//    if (n < min || n > max)
//      s"$n is out of range of $min and $max"
//    else if (n == num) s"you guessed right, the number is $n."
//    else if (n < num) "you guessed wrong, the number is bigger."
//    else "you guessed wrong, the number is smaller."
  def guess[F[_]](n: Int)(implicit MR: MonadReader[F, Config], MW: MonadWriter[F, Vector[String]], MS: MonadState[F, Context]): F[String] = {
    MW.flatMap(MW.tell(Vector(s"n = $n"))) { _ =>
      MR.flatMap(MR.ask) { config =>
        val Config(min, max) = config
        MW.flatMap(MW.tell(Vector(s"min = $min, max = $max"))) { _ =>
          MS.flatMap(MS.get) { context =>
            val Context(Some(num)) = context
            MW.map(MW.tell(Vector(s"num = $num"))) { _ =>
              if (n < min || n > max)
                s"$n is out of range of $min and $max"
              else if (n == num) s"you guessed right, the number is $n."
              else if (n < num) "you guessed wrong, the number is bigger."
              else "you guessed wrong, the number is smaller."
            }
          }
        }
      }
    }
  }
  def liftIO[A](io: IO[A]): GTN[IO, A] = StateT.lift(WriterT.lift(ReaderT.lift(io)))
  def readLine: IO[String] = IO.primitive(StdIn.readLine)
  def randomNum(min: Int, max: Int): IO[Int] = IO.primitive(new util.Random().nextInt(max - min + 1) + min)
  def guessIO(implicit MR: MonadReader[GTN[IO, ?], Config], MW: MonadWriter[GTN[IO, ?], Vector[String]], MS: MonadState[GTN[IO, ?], Context]): GTN[IO, String] = for {
    _ <- liftIO(IO.print("please guess a number: "))
    n <- liftIO(readLine)
    r <- guess[GTN[IO, ?]](n.toInt)
    _ <- liftIO(IO.println(r))
  } yield r
  implicit val MR: MonadReader[GTN[IO, ?], Config] = ???
  implicit val MW: MonadWriter[GTN[IO, ?], Vector[String]] = ???
  implicit val MS: MonadState[GTN[IO, ?], Context] = ???
  guessIO
}