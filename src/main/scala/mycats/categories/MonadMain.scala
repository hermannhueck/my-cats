package mycats.categories

object MonadMain extends App {

  println("\n----- Monad[List]")

  private val ml = Monad[List]
  println(ml.flatMap(List(1, 2, 3))(x => List.fill(x)(x)))

  println("\n----- Monad[Option]")

  private val mo = Monad[Option]
  val f = (x:Int) => if (x % 2 == 0) Option(x) else Option.empty
  println(mo.flatMap(Option(2))(f))
  println(mo.flatMap(Option(1))(f))
  println(mo.flatMap(Option.empty)(f))

  println("\n----- Monad[Option]")

  private val mi = Monad[Id]
  val fi: Id[Int => Int] = (x:Int) => if (x % 2 == 0) x else -x
  println(mi.flatMap(2: Id[Int])(fi))
  println(mi.flatMap(1: Id[Int])(fi))
  println(mi.flatMap(0: Id[Int])(fi))

  println("\n-----\n")
}
