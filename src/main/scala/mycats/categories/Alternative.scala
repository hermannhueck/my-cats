package mycats.categories

import scala.language.higherKinds

trait Alternative[F[_]] extends Any with Applicative[F] with MonoidK[F] { self =>

  def asum[G[_]: Foldable, A](gfa: G[F[A]]): F[A] =
    Foldable[G].fold(gfa)(toMonoid[A])
}

object Alternative {

  def apply[F[_]: Alternative]: Alternative[F] = implicitly[Alternative[F]]
}
