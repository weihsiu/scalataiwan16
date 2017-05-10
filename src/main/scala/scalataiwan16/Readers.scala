package scalataiwan16

import cats._
import cats.data._
import cats.implicits._

object Readers extends App {
  case class Connection()
  val connection = Connection()
  case class User(userName: String, repos: List[String])
  case class Repo(repoName: String, issues: List[String])
  case class Issue(issueId: String)

  def example1 = {
    def getUser(userName: String, connection: Connection): User = ???
    def getRepo(userName: String, repoName: String, connection: Connection): Repo = ???
    def getIssue(userName: String, repoName: String, issueId: String, connection: Connection): Issue = ???
    def getFirstIssueOfFirstRepo(userName: String, connection: Connection): Issue = {
      val user = getUser(userName, connection)
      val repo = getRepo(user.userName, user.repos.head, connection)
      val issue = getIssue(user.userName, repo.repoName, repo.issues.head, connection)
      issue
    }
    val issue: Issue = getFirstIssueOfFirstRepo("weihsiu", connection)
  }

  def example2 = {
    def getUser(userName: String)(connection: Connection): User = ???
    def getRepo(userName: String, repoName: String)(connection: Connection): Repo = ???
    def getIssue(userName: String, repoName: String, issueId: String)(connection: Connection): Issue = ???
    def getFirstIssueOfFirstRepo(userName: String, connection: Connection): Issue = {
      val user = getUser(userName)(connection)
      val repo = getRepo(user.userName, user.repos.head)(connection)
      val issue = getIssue(user.userName, repo.repoName, repo.issues.head)(connection)
      issue
    }
    val issue: Issue = getFirstIssueOfFirstRepo("weihsiu", connection)
  }

  def example3 = {
    def getUser(userName: String): Connection => User = ???
    def getRepo(userName: String, repoName: String): Connection => Repo = ???
    def getIssue(userName: String, repoName: String, issueId: String): Connection => Issue = ???
    def getFirstIssueOfFirstRepo(userName: String)(connection: Connection): Issue = {
      val user = getUser(userName)(connection)
      val repo = getRepo(user.userName, user.repos.head)(connection)
      val issue = getIssue(user.userName, repo.repoName, repo.issues.head)(connection)
      issue
    }
    val issue: Issue = getFirstIssueOfFirstRepo("weihsiu")(connection)
  }

  def example4 = {
    // case class Reader[A, B](run: A => B)
    def getUser(userName: String): Reader[Connection, User] = ???
    def getRepo(userName: String, repoName: String): Reader[Connection, Repo] = ???
    def getIssue(userName: String, repoName: String, issueId: String): Reader[Connection, Issue] = ???
    def getFirstIssueOfFirstRepo(userName: String): Reader[Connection, Issue] = for {
      user <- getUser(userName)
      repo <- getRepo(user.userName, user.repos.head)
      issue <- getIssue(user.userName, repo.repoName, repo.issues.head)
    } yield issue
    val issue: Issue = getFirstIssueOfFirstRepo("weihsiu").run(connection)
  }





  def example5 = {
    def getUser(userName: String): Reader[Connection, Option[User]] = ???
    def getRepo(userName: String, repoName: String): Reader[Connection, Option[Repo]] = ???
    def getIssue(userName: String, repoName: String, issueId: String): Reader[Connection, Option[Issue]] = ???
    def getFirstIssueOfFirstRepo(userName: String): Reader[Connection, Option[Issue]] = for {
      user <- getUser(userName)
      repo <- user match {
        case Some(u) => getRepo(u.userName, u.repos.head)
        case None => None.pure[Reader[Connection, ?]]
      }
      issue <- repo match {
        case Some(r) => getIssue(user.get.userName, r.repoName, r.issues.head)
        case None => None.pure[Reader[Connection, ?]]
      }
    } yield issue

    val issue: Option[Issue] = getFirstIssueOfFirstRepo("weihsiu").run(connection)
  }

  def example6 = {
    type ConnReaderOption[A] = ReaderOption[Connection, A]
    implicit def ReaderOptionMonad[A]: Monad[ConnReaderOption] = new ReaderOptionMonad[Connection]()
    def getUser(userName: String): ConnReaderOption[User] = ???
    def getRepo(userName: String, repoName: String): ConnReaderOption[Repo] = ???
    def getIssue(userName: String, repoName: String, issueId: String): ConnReaderOption[Issue] = ???
    def getFirstIssueOfFirstRepo(userName: String): ConnReaderOption[Issue] = for {
      user <- getUser(userName)
      repo <- getRepo(user.userName, user.repos.head)
      issue <- getIssue(user.userName, repo.repoName, repo.issues.head)
    } yield issue
    val issue: Option[Issue] = getFirstIssueOfFirstRepo("weihsiu").value(connection)
  }






  def example7 = {
    def getUser(userName: String): OptionT[Reader[Connection, ?], User] = ???
    def getRepo(userName: String, repoName: String): OptionT[Reader[Connection, ?], Repo] = ???
    def getIssue(userName: String, repoName: String, issueId: String): OptionT[Reader[Connection, ?], Issue] = ???
    def getFirstIssueOfFirstRepo(userName: String): OptionT[Reader[Connection, ?], Issue] = for {
      user <- getUser(userName)
      repo <- getRepo(user.userName, user.repos.head)
      issue <- getIssue(user.userName, repo.repoName, repo.issues.head)
    } yield issue
    val issue: Option[Issue] = getFirstIssueOfFirstRepo("weihsiu").value.run(connection)
  }

  def example8 = {
    def getUser(userName: String): Reader[Connection, Option[User]] = ???
    def getRepo(userName: String, repoName: String): Reader[Connection, Option[Repo]] = ???
    def getIssue(userName: String, repoName: String, issueId: String): Reader[Connection, Option[Issue]] = ???
    def getFirstIssueOfFirstRepo(userName: String): Reader[Connection, Option[Issue]] = (for {
      user <- OptionT[Reader[Connection, ?], User](getUser(userName))
      repo <- OptionT(getRepo(user.userName, user.repos.head))
      issue <- OptionT(getIssue(user.userName, repo.repoName, repo.issues.head))
    } yield issue).value
    val issue: Option[Issue] = getFirstIssueOfFirstRepo("weihsiu").run(connection)
  }
}
