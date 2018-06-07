package mycats.categories

object ApplicativeMain extends App {

  println("\n----- Applicative[List]")

  private val al = Applicative[List]
  println(al.ap(List((_: Int) + 1))(List(1, 2, 3)))
  println(al.ap2(List((_: Int) + (_: Int)))(List(10, 20, 30), List(1, 2, 3)))
  println(al.map(List(1, 2, 3))(_ + 1))
  println(al.map2(List(10, 20, 30), List(1, 2, 3))(_ + _))
  println(al.tuple2(List(10, 20, 30), List(1, 2, 3)))
  println(al.map3(List(100, 200), List(10, 20), List(1, 2))(_ + _ + _))
  println(al.tuple3(List(100, 200), List(10, 20), List(1, 2)))
  println(al.map4(List(1000, 2000), List(100, 200), List(10, 20), List(1, 2))(_ + _ + _ + _))

  println("\n----- Applicative[Option]")

  private val ao = Applicative[Option]
  println(ao.ap(Option((_: Int) + 1))(Option(1)))
  println(ao.ap2(Option((_: Int) + (_: Int)))(Option(10), Option(1)))
  println(ao.map(Option(1))(_ + 1))
  println(ao.map2(Option(10), Option(1))(_ + _))
  println(ao.tuple2(Option(10), Option(1)))
  println(ao.map3(Option(100), Option(10), Option(1))(_ + _ + _))
  println(ao.tuple3(Option(100), Option(10), Option(1)))
  println(ao.map4(Option(1000), Option(100), Option(10), Option(1))(_ + _ + _ + _))

  println("\n----- Applicative[Id]")

  private val ai = Applicative[Id]
  println(ai.ap(((_: Int) + 1): Id[Int => Int])(1: Id[Int]))

  println("\n----- Composed Applicative")

  val alo = Applicative[List] compose Applicative[Option]
  val lo = alo.map2(List(Option(1), Option.empty[Int], Option(2)), List(Option(2), Option(1)))(_ + _)
  println(lo)

  println("\n-----\n")
}
