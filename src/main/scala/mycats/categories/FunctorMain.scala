package mycats.categories

import mycats.categories.Functor.syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object FunctorMain extends App {

  println("\n----- Functor[List]")

  val l = List(1, 2, 3)
  val l1 = Functor[List].map(l)(_ + 1)
  println(l1)
  val l2 = l.fmap(_ + 1)
  println(l2)

  println("----- Functor[Option]")

  val o = Option(1)
  val o1 = Functor[Option].map(o)(_ + 1)
  println(o1)
  val o2 = o.fmap(_ + 1)
  println(o2)

  println("----- Functor[Id]")

  val i: Id[Int] = 1
  val i1 = Functor[Id].map(i)(_ + 1)
  println(i1)
  val i2 = i.fmap(_ + 1)
  println(i2)

  println("----- Functor[Future]")

  val fut = Future(1)
  val fut1 = Functor[Future].map(fut)(_ + 1)
  Await.ready(fut1, 1.second)
  println(fut1)
  val fut2 = fut.fmap(_ + 1)
  Await.ready(fut2, 1.second)
  println(fut2)

  println("----- Functor[Either]")

  implicit val eitherFunctorSI: Functor[Either[String, ?]] = Functor.eitherFunctor[String]

  val er: Either[String, Int] = Right(1)
  val er1 = eitherFunctorSI.map(er)(_ + 1)
  println(er1)
  val er2 = er.fmap(_ + 1)
  println(er2)

  val el: Either[String, Int] = Left("error")
  val el1 = eitherFunctorSI.map(el)(_ + 1)
  println(el1)
  val el2 = el.fmap(_ + 1)
  println(el2)

  println("----- Functor[Tuple2]")

  implicit val pairFunctorSI: Functor[Tuple2[String, ?]] = Functor.pairFunctor[String]

  val t = ("one", 1)
  val t1 = pairFunctorSI.map(t)(_ + 1)
  println(t1)
  val t2 = t.fmap(_ + 1)
  println(t2)

  println("----- Functor[Function1]")

  implicit val function1FunctorSI: Functor[Function1[String, ?]] = Functor.function1Functor[String]

  val f = (_: String).toInt
  val f1 = function1FunctorSI.map(f)(_ + 1)
  println(f1)
  println(f1("1"))
  val f2 = f.fmap(_ + 1)
  println(f2)
  println(f2("1"))

  println("-----\n")
}
