package scalataiwan16

import cats._
import cats.data._
import cats.implicits._

object Writers extends App {
  case class Connection()
  val connection = Connection()
  case class User(userName: String, repos: List[String])
  case class Repo(repoName: String, issues: List[String])
  case class Issue(issueId: String)

  def example1 = {
    def getUser(userName: String, connection: Connection): User = {
      println(s"getting user $userName")
      ???
    }
    def getRepo(userName: String, repoName: String, connection: Connection): Repo = {
      println(s"getting repo $repoName")
      ???
    }
    def getIssue(userName: String, repoName: String, issueId: String, connection: Connection): Issue = {
      println(s"getting issue $issueId")
      ???
    }
    def getFirstIssueOfFirstRepo(userName: String, connection: Connection): Issue = {
      val user = getUser(userName, connection)
      val repo = getRepo(user.userName, user.repos.head, connection)
      val issue = getIssue(user.userName, repo.repoName, repo.issues.head, connection)
      issue
    }
    val issue: Issue = getFirstIssueOfFirstRepo("weihsiu", connection)
  }

  def example2 = {
    def getUser(userName: String, connection: Connection): (Vector[String], User) = {
      (Vector(s"getting user $userName"), ???)
    }
    def getRepo(userName: String, repoName: String, connection: Connection): (Vector[String], Repo) = {
      (Vector(s"getting repo $repoName"), ???)
    }
    def getIssue(userName: String, repoName: String, issueId: String, connection: Connection): (Vector[String], Issue) = {
      (Vector(s"getting issue $issueId"), ???)
    }
    def getFirstIssueOfFirstRepo(userName: String, connection: Connection): (Vector[String], Issue) = {
      val (l1, user) = getUser(userName, connection)
      val (l2, repo) = getRepo(user.userName, user.repos.head, connection)
      val (l3, issue) = getIssue(user.userName, repo.repoName, repo.issues.head, connection)
      (l1 ++ l2 ++ l3, issue)
    }
    val (ls, issue): (Vector[String], Issue) = getFirstIssueOfFirstRepo("weihsiu", connection)
  }

  def example3 = {
    // case class Writer[L : Monoid, V](run: (L, V))
    def getUser(userName: String, connection: Connection): Writer[Vector[String], User] = for {
      _ <- Writer.tell(Vector(s"getting user $userName"))
    } yield ???
    def getRepo(userName: String, repoName: String, connection: Connection): Writer[Vector[String], Repo] = for {
      _ <- Writer.tell(Vector(s"getting repo $repoName"))
    } yield ???
    def getIssue(userName: String, repoName: String, issueId: String, connection: Connection): Writer[Vector[String], Issue] = for {
      _ <- Writer.tell(Vector(s"getting issue $issueId"))
    } yield ???
    def getFirstIssueOfFirstRepo(userName: String, connection: Connection): Writer[Vector[String], Issue] = for {
      user <- getUser(userName, connection)
      repo <- getRepo(user.userName, user.repos.head, connection)
      issue <- getIssue(user.userName, repo.repoName, repo.issues.head, connection)
    } yield issue
    val (ls, issue): (Vector[String], Issue) = getFirstIssueOfFirstRepo("weihsiu", connection).run
  }






  def example4 = {
    def getUser(userName: String, connection: Connection): Writer[Vector[String], Option[User]] = ???
    def getRepo(userName: String, repoName: String, connection: Connection): Writer[Vector[String], Option[Repo]] = ???
    def getIssue(userName: String, repoName: String, issueId: String, connection: Connection): Writer[Vector[String], Option[Issue]] = ???
    def getFirstIssueOfFirstRepo(userName: String, connection: Connection): Writer[Vector[String], Option[Issue]] = for {
      user <- getUser(userName, connection)
      repo <- user match {
        case Some(u) => getRepo(u.userName, u.repos.head, connection)
        case None => None.pure[Writer[Vector[String], ?]]
      }
      issue <- repo match {
        case Some(r) => getIssue(user.get.userName, r.repoName, r.issues.head, connection)
        case None => None.pure[Writer[Vector[String], ?]]
      }
    } yield issue
    val (ls, issue): (Vector[String], Option[Issue]) = getFirstIssueOfFirstRepo("weihsiu", connection).run
  }

  def example5 = {
    type VectorWriterOption[A] = WriterOption[Vector[String], A]
    implicit def WriterOptionMonad[A]: Monad[VectorWriterOption] = new WriterOptionMonad[Vector[String]]()
    def getUser(userName: String, connection: Connection): VectorWriterOption[User] = ???
    def getRepo(userName: String, repoName: String, connection: Connection): VectorWriterOption[Repo] = ???
    def getIssue(userName: String, repoName: String, issueId: String, connection: Connection): VectorWriterOption[Issue] = ???
    def getFirstIssueOfFirstRepo(userName: String, connection: Connection): VectorWriterOption[Issue] = for {
      user <- getUser(userName, connection)
      repo <- getRepo(user.userName, user.repos.head, connection)
      issue <- getIssue(user.userName, repo.repoName, repo.issues.head, connection)
    } yield issue
    val (ls, issue): (Vector[String], Option[Issue]) = getFirstIssueOfFirstRepo("weihsiu", connection).value.run
  }






  def example6 = {
    def getUser(userName: String, connection: Connection): OptionT[Writer[Vector[String], ?], User] = ???
    def getRepo(userName: String, repoName: String, connection: Connection): OptionT[Writer[Vector[String], ?], Repo] = ???
    def getIssue(userName: String, repoName: String, issueId: String, connection: Connection): OptionT[Writer[Vector[String], ?], Issue] = ???
    def getFirstIssueOfFirstRepo(userName: String, connection: Connection): OptionT[Writer[Vector[String], ?], Issue] = for {
      user <- getUser(userName, connection)
      repo <- getRepo(user.userName, user.repos.head, connection)
      issue <- getIssue(user.userName, repo.repoName, repo.issues.head, connection)
    } yield issue
    val (ls, issue): (Vector[String], Option[Issue]) = getFirstIssueOfFirstRepo("weihsiu", connection).value.run
  }

  def example7 = {
    def getUser(userName: String, connection: Connection): Writer[Vector[String], Option[User]] = ???
    def getRepo(userName: String, repoName: String, connection: Connection): Writer[Vector[String], Option[Repo]] = ???
    def getIssue(userName: String, repoName: String, issueId: String, connection: Connection): Writer[Vector[String], Option[Issue]] = ???
    def getFirstIssueOfFirstRepo(userName: String, connection: Connection): Writer[Vector[String], Option[Issue]] = (for {
      user <- OptionT(getUser(userName, connection))
      repo <- OptionT(getRepo(user.userName, user.repos.head, connection))
      issue <- OptionT(getIssue(user.userName, repo.repoName, repo.issues.head, connection))
    } yield issue).value
    val (ls, issue): (Vector[String], Option[Issue]) = getFirstIssueOfFirstRepo("weihsiu", connection).run
  }
}
