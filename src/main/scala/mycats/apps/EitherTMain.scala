package mycats.apps

import mycats.categories.Functor
import mycats.transformers.EitherT

object EitherTMain extends App {

  println("\n-----")

  val lei: List[Either[String, Int]] = List(Left("failed"), Right(1))
  val etli: EitherT[List, String, Int] = EitherT(lei)

  val mapped = Functor[EitherT[List, String, ?]].map(etli)(_ + 1)
  println(mapped)

  val weird: EitherT[List, String, List[Int]] = EitherT(List(Left("failed"), Right(List(1, 2, 3)), Right(List(4, 5, 6))))
  println(weird)

  val weirdMapped: EitherT[List, String, List[Int]] = Functor[EitherT[List, String, ?]].map(weird)(li => li.map(_ + 1))
  println(weirdMapped)

  val fComposed = Functor[EitherT[List, String, ?]] compose Functor[List]
  println(fComposed)

  val weirdMapped2: EitherT[List, String, List[Int]] = fComposed.map(weird)(_ + 1)
  println(weirdMapped2)

  val xs = List(Left("x failed"), Right(1), Right(2), Right(3))
  val ys = List(Left("y failed"), Right(10), Right(20))

  val xys = for {
    x <- EitherT(xs)
    y <- EitherT(ys)
  } yield x + y

  println(xys)

  println("-----\n")
}
