package mycats.apps

import mycats.categories.Functor.ops._
import mycats.categories.{Functor, Id}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object FunctorMain extends App {

  println("\n----- Functor[List]")

  val l = List(1, 2, 3)
  val l1 = l.map(_ + 1)
  println(l1)
  val l2 = l.fmap(_ + 1)
  println(l2)

  println("----- Functor[Option]")

  val o = Option(1)
  val o1 = o.map(_ + 1)
  println(o1)
  val o2 = o.fmap(_ + 1)
  println(o2)

  println("----- Functor[Id]")

  val i: Id[Int] = 1
  val i1 = i.map(_ + 1)
  println(i1)
  val i2 = i.fmap(_ + 1)
  println(i2)

  println("----- Functor[Future]")

  val fut = Future(1)
  val fut1 = fut.map(_ + 1)
  Await.ready(fut1, 1.second)
  println(fut1)
  val fut2 = fut.fmap(_ + 1)
  Await.ready(fut2, 1.second)
  println(fut2)

  println("----- Functor[Either[L, ?]]")

  val er: Either[String, Int] = Right(1)
  val er1 = er.map(_ + 1)
  println(er1)
  val er2 = er.fmap(_ + 1)
  println(er2)

  val el: Either[String, Int] = Left("error")
  val el1 = el.map(_ + 1)
  println(el1)
  val el2 = el.fmap(_ + 1)
  println(el2)

  println("----- Functor[Tuple2[L, ?]]")

  val t = ("one", 1)
  val t1 = t.map(_ + 1)
  println(t1)
  val t2 = t.fmap(_ + 1)
  println(t2)

  println("----- Functor[Function1[L, ?]]")

  val fn: String => Int = (x: String) => x.toInt
  val fn1 = fn.map(_ + 1)
  println(fn1)
  println(fn1("1"))
  val fn2 = fn.fmap(_ + 1)
  println(fn2)
  println(fn2("1"))

  val ffn = Functor[String => ?]
  val len: String => Int = _.length
  val rfn = ffn.map(len)(_ + 10)
  println(rfn)
  println(rfn("hello"))

  println("-----\n")
}
