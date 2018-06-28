package mycats.categories

trait Monoid[A] extends Any with Semigroup[A] {

  def empty: A
}

object Monoid {

  def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]

  abstract class MonoidTemplate[A](zero: A, f: (A, A) => A) extends Monoid[A] {
    override def empty: A = zero
    override def combine(x: A, y: A): A = f(x, y)
  }

  implicit val stringMonoid: Monoid[String] = new MonoidTemplate[String]("", _ + _) {}

  implicit val intMonoid: Monoid[Int] = new MonoidTemplate[Int](0, _ + _) {}
}
