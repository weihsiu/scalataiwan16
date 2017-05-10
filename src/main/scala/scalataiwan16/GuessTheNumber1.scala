package scalataiwan16

import scala.io.StdIn
import scala.language.higherKinds

import aiyou._
import aiyou.implicits._
import cats._
import cats.data._
import cats.implicits._

object GuessTheNumber1 extends App {
  case class Config(min: Int, max: Int)
  case class Context(num: Option[Int])
  type GTN[F[_], A] = StateT[WriterT[ReaderT[F, Config, ?], Vector[String], ?], Context, A]
  def guess[F[_] : Monad](n: Int): GTN[F, String] = for {
    _ <- StateT.lift(WriterT.tell[ReaderT[F, Config, ?], Vector[String]](Vector(s"n = $n")))
    config <- StateT.lift(WriterT.lift[ReaderT[F, Config, ?], Vector[String], Config](ReaderT.ask))
    Config(min, max) = config
    _ <- StateT.lift(WriterT.tell[ReaderT[F, Config, ?], Vector[String]](Vector(s"min = $min, max = $max")))
    context <- StateT.get[WriterT[ReaderT[F, Config, ?], Vector[String], ?], Context]
    Context(Some(num)) = context
    _ <- StateT.lift(WriterT.tell[ReaderT[F, Config, ?], Vector[String]](Vector(s"num = $num")))
  } yield
      if (n < min || n > max)
        s"$n is out of range of $min and $max"
      else if (n == num) s"you guessed right, the number is $n."
      else if (n < num) "you guessed wrong, the number is bigger."
      else "you guessed wrong, the number is smaller."
  def liftIO[A](io: IO[A]): GTN[IO, A] = StateT.lift(WriterT.lift(ReaderT.lift(io)))
  def readLine: IO[String] = IO.primitive(StdIn.readLine)
  def randomNum(min: Int, max: Int): IO[Int] = IO.primitive(new util.Random().nextInt(max - min + 1) + min)
  def guessIO: GTN[IO, String] = for {
    _ <- liftIO(IO.print("please guess a number: "))
    n <- liftIO(readLine)
    r <- guess[IO](n.toInt)
    _ <- liftIO(IO.println(r))
  } yield r
  def guessTheNumber: GTN[IO, Unit] = for {
    r <- guessIO
    _ <- if (r.startsWith("you guessed right")) ().pure[GTN[IO, ?]] else guessTheNumber
  } yield ()
  def startGame: GTN[IO, Unit] = for {
    config <- StateT.lift(WriterT.lift[ReaderT[IO, Config, ?], Vector[String], Config](ReaderT.ask))
    Config(min, max) = config
    num <- liftIO(randomNum(min, max))
    _ <- StateT.set[WriterT[ReaderT[IO, Config, ?], Vector[String], ?], Context](Context(Some(num)))
    _ <- guessTheNumber
  } yield ()
  println(startGame.run(Context(None)).run.run(Config(0, 100)).unsafePerformIO())
//  println(guess[Eval](42).runA(Context(42)).run.run(Config(0, 100)).value)
}
