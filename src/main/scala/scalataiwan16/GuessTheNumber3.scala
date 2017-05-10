package scalataiwan16

import aiyou._
import aiyou.implicits._
import cats._
import cats.data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import IOEffect._
import scala.io.StdIn

object GuessTheNumber3 extends App {
  case class Config(min: Int, max: Int)
  case class Context(num: Option[Int])
  type ConfigReader[A] = Reader[Config, A]
  type LogWriter[A] = Writer[Vector[String], A]
  type ContextState[A] = State[Context, A]
  type GTN = Fx.fx5[ConfigReader, LogWriter, ContextState, Eval, IO]
  type _configReader[R] = ConfigReader |= R
  type _logWriter[R] = LogWriter |= R
  type _contextState[R] = ContextState |= R
  def guess[R : _configReader : _logWriter : _contextState : _eval](n: Int): Eff[R, String] = for {
    _ <- tell(Vector(s"n = $n"))
    config <- ask[R, Config]
    Config(min, max) = config
    _ <- tell(Vector(s"min = $min, max = $max"))
    context <- get[R, Context]
    Context(Some(num)) = context
    _ <- tell(Vector(s"num = $num"))
  } yield
      if (n < min || n > max)
        s"$n is out of range of $min and $max"
      else if (n == num) s"you guessed right, the number is $n."
      else if (n < num) "you guessed wrong, the number is bigger."
      else "you guessed wrong, the number is smaller."
  def readLine: IO[String] = IO.primitive(StdIn.readLine)
  def randomNum(min: Int, max: Int): IO[Int] = IO.primitive(new util.Random().nextInt(max - min + 1) + min)
  def guessIO[R : _configReader : _logWriter : _contextState : _eval : _io]: Eff[R, String] = for {
    _ <- fromIO(IO.print("please guess a number: "))
    n <- fromIO(readLine)
    r <- guess(n.toInt)
    _ <- fromIO(IO.println(r))
  } yield r
  def guessTheNumber[R : _configReader : _logWriter : _contextState : _eval : _io]: Eff[R, Unit] = for {
    r <- guessIO
    _ <- if (r.startsWith("you guessed right")) ().pureEff[R] else guessTheNumber
  } yield ()
  def startGame[R : _configReader : _logWriter : _contextState : _eval : _io]: Eff[R, Unit] = for {
    config <- ask[R, Config]
    Config(min, max) = config
    num <- fromIO(randomNum(min, max))
    _ <- put[R, Context](Context(Some(num)))
    _ <- guessTheNumber
  } yield ()
  val resultIO = startGame[GTN].runReader(Config(0, 100)).runWriter.runState(Context(None)).runEval.detach
  println(resultIO.unsafePerformIO)
}
