package mycats.categories

trait Monoid[A] extends Any with Semigroup[A] {

  def empty: A
}

object Monoid {

  def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]
}
