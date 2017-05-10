package scalataiwan16

import cats._
import cats.data._
import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.future._
import org.atnos.eff.all.{none => _, _}
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.future._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object EffSandbox extends App {
  def example1 = {
    type LogWriter[A] = Writer[String, A]
    type _logWriter[R] = LogWriter |= R
    type Stack = Fx.fx2[LogWriter, Option]

    def program[R : _logWriter : _option]: Eff[R, Int] = for {
      _ <- tell("hello")
      n <- fromOption(123.some)
      m <- fromOption(none[Int])
      _ <- tell(s"world $n")
    } yield n

    println(program[Stack].runWriter.runOption.run)
    println(program[Stack].runOption.runWriter.run)
  }
  def example2 = {
    import scala.concurrent.ExecutionContext.Implicits.global
    implicit val scheduler = ExecutorServices.schedulerFromGlobalExecutionContext
    type Stack = Fx.fx2[TimedFuture, Option]

    def number: Future[Option[Int]] = Future.successful(123.some)

    def program[R : _future : _option]: Eff[R, Int] = for {
      r <- fromFuture(number)
      n <- fromOption(r)
    } yield n

    println(Await.result(program[Stack].runOption.runSequential, 5.seconds))
//    println(program[Stack].runSequential.runOption.run)
  }
  example2
}
