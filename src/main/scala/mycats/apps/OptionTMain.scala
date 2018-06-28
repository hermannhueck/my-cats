package mycats.apps

import mycats.categories.Monad.ops._
import mycats.categories.{Id, Monad}
import mycats.transformers.OptionT

import scala.language.higherKinds

object OptionTMain extends App {

  println("\n-----")

  val loi: List[Option[Int]] = List(Some(1), None, Some(2), Some(3))
  val otli: OptionT[List, Int] = OptionT(loi)

  val mapped = Monad[OptionT[List, ?]].map(otli) { _.toString + "!" }
  println(mapped)

  val flatMapped1 = Monad[OptionT[List, ?]].flatMap(otli) { x => OptionT[List, String](List.fill(x)(Option(x.toString))) }
  println(flatMapped1)

  val flatMapped2 = otli.flatMap { x => OptionT[List, String](List.fill(x)(Option(x.toString))) }
  println(flatMapped2)

  val flatMappedF = otli.flatMapF { x => List.fill(x)(Option(x.toString)) }
  println(flatMappedF)

  val isDefined = otli.isDefined
  println(isDefined)

  val isEmpty = otli.isEmpty
  println(isEmpty)

  val got = otli.getOrElse(42)
  println(got)

  println("\n----- For comprehension with OptionT[List, Int] encapsulating List[Option[Int]]")

  val result1: OptionT[List, Int] = for {
    x <- otli
    y <- otli
  } yield x * y

  println(result1)
  println(result1.value)

  println("\n----- Generic Processing of Monads")

  def processIntMonads[F[_]: Monad](monad1: F[Int], monad2: F[Int]): F[Int] =
    for {
      x <- monad1
      y <- monad2
    } yield x * y

  val result2 = processIntMonads(otli, otli) // OptionT[List, Int]
  println(result2.value)

  val otvi = OptionT[Vector, Int](Vector(Option(3), Option(5))) // OptionT[List, Int]
  println(processIntMonads(otvi, otvi).value)

  val otoi = OptionT[Option, Int](Option(Option(5))) // OptionT[Vector, Int]
  println(processIntMonads(otoi, otoi).value)

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import scala.concurrent.{Await, Future}

  val otfi = processIntMonads(OptionT(Future(Option(5))), OptionT(Future(Option(5)))) // OptionT[Option, Int]
  Await.ready(otfi.value, 1.second)
  println(otfi.value)

  val ii = processIntMonads(5: Id[Int], 5: Id[Int])
  println(ii)

  val otii = processIntMonads(OptionT[Id, Int](Option(5)), OptionT[Id, Int](Option(5))) // OptionT[Id, Int]
  println(otii.value)

  println("-----\n")
}
