package mycats.categories

import scala.language.higherKinds

trait SemigroupK[F[_]] extends Any {

  def combineK[A](x: F[A], y: F[A]): F[A]
  // op: <+>

  def toSemigroup[A]: Semigroup[F[A]] = new Semigroup[F[A]] {
    override def combine(x: F[A], y: F[A]): F[A] = combineK(x, y)
  }

  def algebra[A]: Semigroup[F[A]] = toSemigroup[A]
}

object SemigroupK {

  def apply[F[_]: SemigroupK]: SemigroupK[F] = implicitly[SemigroupK[F]]
}
