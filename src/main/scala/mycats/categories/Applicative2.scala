package mycats.categories

import scala.language.higherKinds

// Applicative normally implements map, map2, product etc
// in terms of ap and pure. Here we go the other way round:
// We add pure and implement map2 and ap in terms of map and product.
//
trait Applicative2[F[_]] extends Any with Functor[F] { self =>

  def pure[A](a: A): F[A]

  override def map[A, B](fa: F[A])(f: A => B): F[B] // already defined in Functor

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  // --- implemented in terms of the above

  // map2 in terms of map and product
  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    map(product(fa, fb)) { case (a, b) => f(a, b) }

  // ap implemented in terms of map2
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = map2(ff, fa)((f, a) => f(a))

  def <*>[A, B](ff: F[A => B])(fa: F[A]): F[B] = ap(ff)(fa) // alias for ap

  def tuple2[A, B](fa: F[A], fb: F[B]): F[(A, B)] = product(fa, fb)  // alias for product

  // for map3, map4 etc and tuple3, tuple4 etc ... see trait Applicative
}