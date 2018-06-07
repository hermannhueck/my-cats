package mycats.categories

trait Semigroup[A] extends Any {

  def combine(x: A, y: A): A

  //def |+|(x: A, y: A): A = combine(x, y)
}

object Semigroup {

  def apply[A: Semigroup]: Semigroup[A] = implicitly[Semigroup[A]]
}
