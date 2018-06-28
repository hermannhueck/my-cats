package mycats.apps

import mycats.categories.Monad.ops._
import mycats.categories.{Id, Monad}

object MonadFunction1Main extends App {

  println("\n----- Monad[Function1[String, ?]] (1/3)")

  {
    val f: Int => Int = (_: Int) * 2
    val g: Int => Int = (_: Int) + 10

    val addStuff: Int => Int = for {
      a <- f
      b <- g
    } yield a + b

    println(addStuff(3))
  }

  println("\n----- Monad[Function1[String, ?]] (2/3): Example from 'Scala with Cats'")

  {
    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )
    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )

    case class Db(usernames: Map[Int, String], passwords: Map[String, String])
    val db = Db(users, passwords)

    type DbReader[A] = Function1[Db, A]

    def findUsername(userId: Int): DbReader[Option[String]] =
      db => db.usernames.get(userId)

    def checkPassword(username: String, password: String): DbReader[Boolean] =
      db => db.passwords.get(username).contains(password)

    def checkLogin(userId: Int, password: String): DbReader[Boolean] = // ^= Reader[Db, Boolean] ^= Kleisli[Id, Db, Boolean]
      for {
        optUsername <- findUsername(userId)
        passwordOk <- optUsername
          .map(un => checkPassword(un, password))
          .getOrElse((_:Db) => false)
          // .getOrElse(Monad[DbReader].pure(false))
      } yield passwordOk

    println(checkLogin(1, "zerocool").apply(db))
    println(checkLogin(4, "davinci").apply(db))

    println(checkLogin(1, "zerocool")(db))
    println(checkLogin(4, "davinci")(db))
  }

  println("\n----- Monad[Function1[String, ?]] (3/3): Example from 'Herding Cats'")

  {
    case class User(id: Long, parentId: Long, name: String, email: String)

    trait UserRepo {
      def get(id: Long): User
      def find(name: String): User
    }

    trait Users {

      def getUser(id: Long): UserRepo => User = repo => repo.get(id)

      def findUser(name: String): UserRepo => User = repo => repo.find(name)
    }

    object UserInfo extends Users {

      def userInfo(name: String): UserRepo => Map[String, String] =
        for {
          user <- findUser(name)
          boss <- getUser(user.parentId)
        } yield Map(
          "name" -> s"${user.name}",
          "email" -> s"${user.email}",
          "boss_name" -> s"${boss.name}"
        )
    }

    trait Program {

      def app: UserRepo => String =
        for {
          fredo <- UserInfo.userInfo("Fredo")
        } yield fredo.toString
    }

    val testUsers = List(User(
      0, 0, "Vito", "vito@example.com"),
      User(1, 0, "Michael", "michael@example.com"),
      User(2, 0, "Fredo", "fredo@example.com"
      ))

    object Main extends Program {

      def run: String = app(mkUserRepo(testUsers))

      def mkUserRepo(users: List[User]): UserRepo = new UserRepo {
        def get(id: Long): User = users.find(_.id == id).get
        def find(name: String): User = users.find(_.name == name).get
      }
    }

    println(Main.run)
  }

  println("\n-----\n")
}
