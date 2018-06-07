package mycats.categories

import scala.language.higherKinds

trait MonoidK[F[_]] extends Any with SemigroupK[F] { self =>

  def empty[A]: F[A]

  def toMonoid[A]: Monoid[F[A]] = new Monoid[F[A]] {
    override def empty: F[A] = self.empty[A]
    override def combine(x: F[A], y: F[A]): F[A] = combineK(x, y)
  }

  override def algebra[A]: Monoid[F[A]] = toMonoid[A]
}

object MonoidK {

  def apply[F[_]: MonoidK]: MonoidK[F] = implicitly[MonoidK[F]]
}
