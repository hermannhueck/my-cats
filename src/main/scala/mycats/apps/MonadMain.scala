package mycats.apps

import mycats.categories.{Id, Monad}
import Monad.ops._

object MonadMain extends App {

  println("\n----- Monad[List]")

  private val ml = Monad[List]
  println(ml.flatMap(List(1, 2, 3))(x => List.fill(x)(x)))
  println(
    for {
      i1 <- List(1, 2, 3)
      i2 <- List.fill(i1)(i1)
    } yield i2 * 10
  )

  println("\n----- Monad[Option]")

  private val mo = Monad[Option]
  val fo = (x:Int) => if (x % 2 == 0) Option(x) else Option.empty
  println(mo.flatMap(Option(2))(fo))
  println(mo.flatMap(Option(1))(fo))
  println(mo.flatMap(Option.empty)(fo))
  println(
    for {
      i1 <- Option(2)
      i2 <- fo(i1)
    } yield i2 + "!"
  )

  println("\n----- Monad[Id]")

  private val mi = Monad[Id]
  val fi: Id[Int => Int] = (x:Int) => if (x % 2 == 0) x else -x
  println(mi.flatMap(2: Id[Int])(fi))
  println(mi.flatMap(1: Id[Int])(fi))
  println(mi.flatMap(0: Id[Int])(fi))
  println(
    for {
      i1 <- 1: Id[Int]
      i2 <- if (i1 % 2 == 0) 22: Id[Int] else 21: Id[Int]
    } yield i2 * 10
  )

  println("\n----- Monad[Either[String, ?]]")

  private val me = Monad[Either[String, ?]]
  val fe: Int => Either[String, Int] = (x:Int) => if (x % 2 == 0) Right(x) else Left("odd")
  println(me.flatMap(Right(2))(fe))
  println(me.flatMap(Right(1))(fe))
  println(me.flatMap(Left("error"))(fe))
  println(
    for {
      i1 <- Right(2)
      i2 <- fe(i1)
    } yield i2 + "!"
  )

  println("\n----- Monad[Tuple2[String, ?]]")

  private val mt = Monad[Tuple2[String, ?]]
  val ft: Int => Tuple2[String, Int] = (x:Int) => ("tupled", x + 1)
  println(mt.flatMap(("start-", 1))(ft))

  println("\n----- Monad[Function1[String, ?]]")

  {
    val f: Int => Int = (_: Int) * 2
    val g: Int => Int = (_: Int) + 10

    val addStuff: Int => Int = for {
      a <- f
      b <- g
    } yield a + b

    println(addStuff(3))
  }

  println("\n-----\n")
}
