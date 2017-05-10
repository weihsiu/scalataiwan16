package scalataiwan16

import aiyou._
import aiyou.implicits._
import cats._
import cats.data._
import cats.implicits._
import org.atnos.eff.{ ExecutorServices, _ }
import org.atnos.eff.all._
import org.atnos.eff.future._
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.future._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.io.{ Source, StdIn }
import IOEffect._
import Terminals._

object Hangman extends App {
  implicit val scheduler = ExecutorServices.schedulerFromScheduledExecutorService(ExecutorServices.scheduledExecutor(10))
  case class Config(minWordLen: Int, maxWordLen: Int, cheat: Boolean)
  case class Context(word: String, guesses: List[Char])
  trait Result
  case object Continue extends Result
  case object YouWin extends Result
  case object YouLose extends Result
  type ConfigReader[A] = Reader[Config, A]
  type ContextState[A] = State[Context, A]
  type Stack = Fx.fx4[ConfigReader, ContextState, TimedFuture, IO]
  type _config[R] = ConfigReader |= R
  type _context[R] = ContextState |= R
  def readLine: IO[String] = IO.primitive(StdIn.readLine)
  def readFile(file: String): IO[List[String]] = IO.primitive(Source.fromFile(file).getLines.toList)
  def randomNum(min: Int, max: Int): IO[Int] = IO.primitive(new util.Random().nextInt(max - min + 1) + min)
//  def randomWord(len: Int): Future[String] = Future(Source.fromURL(s"http://randomword.setgetgo.com/get.php?len=$len").mkString)
  def randomWord(len: Int): Future[String] = Future(List("hello", "world", "scala", "taiwan")(new util.Random().nextInt(4)))
  def randomWordEff[R : _config : _future : _io]: Eff[R, String] = for {
    config <- ask[R, Config]
    Config(min, max, _) = config
    len <- fromIO(randomNum(min, max))
    word <- fromFuture(randomWord(len))
  } yield word.toUpperCase
  def outputFile(col: Int, row: Int, file: String): IO[Unit] = for {
    ls <- readFile(file)
    _ <- ls.zipWithIndex.map({ case (l, i) => writeText(col, row + i, l) }).sequence
  } yield ()
  def outputImage(word: String, guesses: List[Char]): IO[Unit] =
    outputFile(0, 8, s"${numMisses(word, guesses)}-miss.txt")
  def outputStatus(word: String, guesses: List[Char]): IO[Unit] = for {
    _ <- writeText(10, 9, makes(word, guesses))
    _ <- writeText(10, 10, List.fill(word.size)('-').mkString(" "))
    _ <- writeText(10, 12, s"Misses: ${misses(word, guesses)}")
    _ <- calculateResult(word, guesses) match {
      case Continue => IO.pure(())
      case YouWin => writeText(10, 14, "You win!!\n")
      case YouLose => writeText(10, 14, s"The word is $word.  You Lose.\n")
    }
  } yield ()
  def makes(word: String, guesses: List[Char]): String =
    word.flatMap(c => if (guesses.contains(c)) s"$c " else "  ")
  def numMisses(word: String, guesses: List[Char]): Int =
    guesses.filterNot(c => word.contains(c.toString)).size
  def misses(word: String, guesses: List[Char]): String =
    guesses.filterNot(c => word.contains(c.toString)).reverse.mkString(" ")
  def calculateResult(word: String, guesses: List[Char]): Result =
    if (word.toSet == guesses.toSet.intersect(word.toSet)) YouWin
    else if (numMisses(word, guesses) >= 6) YouLose
    else Continue
  def outputScreen[R : _config : _context : _io]: Eff[R, Unit] = for {
    _ <- fromIO(clearScreen)
    context <- get[R, Context]
    Context(word, guesses) = context
    _ <- fromIO(outputImage(word, guesses))
    _ <- fromIO(outputStatus(word, guesses))
    config <- ask[R, Config]
    _ <- if (config.cheat) fromIO(writeText(0, 20, word)) else Eff.pure[R, Unit](())
  } yield ()
  def gameLoop[R : _config : _context : _io]: Eff[R, Unit] = for {
    _ <- outputScreen
    key <- fromIO(readKey)
    context <- get[R, Context]
    _ <- fromIO(writeText(0, 20, context.toString))
    _ <- put[R, Context](context.copy(guesses = key.char.toUpper :: context.guesses))
    context2 <- get[R, Context]
    Context(word, guesses) = context2
    _ <- calculateResult(word, guesses) match {
      case Continue => gameLoop
      case YouWin | YouLose => Eff.pure[R, Unit](())
    }
  } yield ()
  def startGame[R : _config : _context : _future : _io]: Eff[R, Unit] = for {
    word <- randomWordEff
    _ <- put[R, Context](Context(word, List()))
    result <- gameLoop
    _ <- outputScreen
  } yield ()
  Await.result(startGame[Stack].runReader(Config(5, 10, false)).runState(Context("", List())).runIO.runSequential, Duration.Inf)
}
