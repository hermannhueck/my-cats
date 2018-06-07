package mycats.categories

import scala.concurrent.Future
import scala.language.higherKinds

trait Monad[F[_]] extends Any with Applicative[F] { self =>

  // intrinsic abstract Applicative methods

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]


  // method implementations in terms of pure and flatMap

  def >>=[A, B](fa: F[A])(f: A => F[B]): F[B] = flatMap(fa)(f) // alias for flatMap

  override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    flatMap(ff)((f: A => B) => map(fa)(f))

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))

  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)
}

object Monad {

  def apply[F[_]: Monad]: Monad[F] = implicitly[Monad[F]]

  // default typeclass instances in implicit scope

  implicit def listMonad: Monad[List] = new Monad[List] {
    override def pure[A](a: A): List[A] = List(a)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  implicit def optionMonad: Monad[Option] = new Monad[Option] {
    override def pure[A](a: A): Option[A] = Option(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

  implicit def vectorMonad: Monad[Vector] = new Monad[Vector] {
    override def pure[A](a: A): Vector[A] = Vector(a)
    override def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] = fa.flatMap(f)
  }

  implicit def futureMonad: Monad[Future] = new Monad[Future] {
    import scala.concurrent.ExecutionContext.Implicits.global
    override def pure[A](a: A): Future[A] = Future(a)
    override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)
  }

  implicit def idMonad: Monad[Id] = new Monad[Id] {
    override def pure[A](a: A): Id[A] = a
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }

  object syntax {

    implicit class PimpedF[F[_]: Monad, A](m: F[A]) {
      private val F = implicitly[Monad[F]]
      def pure[A](i: A): F[A] = F.pure(i)
      def flatMap(f: A => F[A]): F[A] = F.flatMap(m)(f)
      def map(f: A => A): F[A] = F.map(m)(f)
    }
  }
}
