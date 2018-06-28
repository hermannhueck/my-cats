package mycats.apps

import mycats.categories.Applicative.ops._
import mycats.categories.{Applicative, Id}

object ApplicativeMain extends App {

  val plus1: Int => Int = _ + 1
  private val add2: (Int, Int) => Int = _ + _
  private val add3: (Int, Int, Int) => Int = _ + _ + _
  private val add4: (Int, Int, Int, Int) => Int = _ + _ + _ + _

  println("\n----- Applicative[List]")

  private val al = Applicative[List]
  println(List(1,2,3).ap(List(plus1))) // same as:
  println(al.ap(List(plus1))(List(1, 2, 3)))
  println(al.ap2(List(add2))(List(10, 20, 30), List(1, 2, 3)))
  println(al.map(List(1, 2, 3))(plus1))
  println(al.map2(List(10, 20, 30), List(1, 2, 3))(add2))
  println(al.tuple2(List(10, 20, 30), List(1, 2, 3)))
  println(al.map3(List(100, 200), List(10, 20), List(1, 2))(add3))
  println(al.tuple3(List(100, 200), List(10, 20), List(1, 2)))
  println(al.map4(List(1000, 2000), List(100, 200), List(10, 20), List(1, 2))(add4))

  println("\n----- Applicative[Option]")

  private val ao = Applicative[Option]
  println(Option(1).ap(Option(plus1))) // same as:
  println(ao.ap(Option(plus1))(Option(1)))
  println(ao.ap2(Option(add2))(Option(10), Option(1)))
  println(ao.map(Option(1))(plus1))
  println(ao.map2(Option(10), Option(1))(add2))
  println(ao.tuple2(Option(10), Option(1)))
  println(ao.map3(Option(100), Option(10), Option(1))(add3))
  println(ao.tuple3(Option(100), Option(10), Option(1)))
  println(ao.map4(Option(1000), Option(100), Option(10), Option(1))(add4))

  println("\n----- Applicative[Id]")

  private val ai = Applicative[Id]
  println(ai.ap(plus1: Id[Int => Int])(1: Id[Int])) // same as:
  println((1: Id[Int]).ap(plus1: Id[Int => Int]))

  println("\n----- Composed Applicative")

  val alo = Applicative[List] compose Applicative[Option]
  val lo = alo.map2(List(Option(1), Option.empty[Int], Option(2)), List(Option(2), Option(1)))(add2)
  println(lo)

  println("\n----- Applicative[Either[L, ?]]")

  private val ae = Applicative[Either[String, ?]]

  println((Right(1): Either[String, Int]).ap(Right(plus1))) // same as:
  println(ae.ap(Right(plus1))(Right(1)))
  println(ae.ap(Left("function error + "))(Right(1)))
  println(ae.ap(Right(plus1))(Left("value error")))
  println(ae.ap(Left("function error + "))(Left("value error")))

  println(ae.ap2(Right(add2))(Right(10), Right(1)))
  println(ae.map(Right(1))(plus1))
  println(ae.map2(Right(10), Right(1))(add2))
  println(ae.tuple2(Right(10), Right(1)))
  println(ae.map3(Right(100), Right(10), Right(1))(add3))
  println(ae.tuple3(Right(100), Right(10), Right(1)))
  println(ae.map4(Right(1000), Right(100), Right(10), Right(1))(add4))

  println("\n----- Applicative[Tuple2[L, ?]]")

  private val at = Applicative[Tuple2[String, ?]]
  println(("right", 1).ap(("left-", plus1))) // same as:
  println(at.ap(("left-", plus1))(("right", 1)))

//  println("\n----- Applicative[Function1[L, ?]]")
//
//  val len: String => Int = _.length
//  val plus10: Int => Int = _ + 10
//  val x: String => Int => Int = s2i => s => s2i(s) + 10
//
//  val ffn = Applicative[String => ?]
//  val rfn1 = ffn.map(len)(plus10)
//  println(rfn1)
//  println(rfn1("hello"))
//  val rfn2 = ffn.ap(x)(len)
//  println(rfn2)
//  println(rfn2("hello"))

  println("\n-----\n")
}
