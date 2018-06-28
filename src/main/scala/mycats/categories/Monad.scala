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
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa flatMap f
  }

  implicit def optionMonad: Monad[Option] = new Monad[Option] {
    override def pure[A](a: A): Option[A] = Option(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f
  }

  implicit def vectorMonad: Monad[Vector] = new Monad[Vector] {
    override def pure[A](a: A): Vector[A] = Vector(a)
    override def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] = fa flatMap f
  }

  implicit def futureMonad: Monad[Future] = new Monad[Future] {
    import scala.concurrent.ExecutionContext.Implicits.global
    override def pure[A](a: A): Future[A] = Future(a)
    override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa flatMap f
  }

  implicit def idMonad: Monad[Id] = new Monad[Id] {
    override def pure[A](a: A): Id[A] = a
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }

  implicit def eitherMonad[L]: Monad[Either[L, ?]] = new Monad[Either[L, ?]] {
    override def pure[R](r: R): Either[L, R] = Right(r)
    override def flatMap[R1, R2](fa: Either[L, R1])(f: R1 => Either[L, R2]): Either[L, R2] = fa flatMap f
  }

  implicit def tuple2Monad[L: Monoid]: Monad[Tuple2[L, ?]] = new Monad[Tuple2[L, ?]] {
    override def pure[R](r: R): (L, R) = (Monoid[L].empty, r)
    override def flatMap[R1, R2](fa: (L, R1))(f: R1 => (L, R2)): (L, R2) = fa match {
      case (l1, r1) =>
        val (l2, r2) = f(r1)
        val lCombined = Monoid[L].combine(l1, l2)
        (lCombined, r2)
    }
  }

  implicit def function1Monad[P]: Monad[P => ?] = new Monad[P => ?] {

    override def pure[R](r: R): P => R = _ => r

    override def flatMap[R1, R2](fa: P => R1)(f: R1 => P => R2): P => R2 =
      p => f(fa(p))(p)

    override def map[R1, R2](fa: P => R1)(f: R1 => R2): P => R2 =
      fa andThen f
  }

  object ops {

    implicit class MonadF[F[_]: Monad, A](ctx: F[A]) {

      private val F = implicitly[Monad[F]]

      def pure(a: A): F[A] = F.pure(a)
      def flatMap[B](f: A => F[B]): F[B] = F.flatMap(ctx)(f)
      def map[B](f: A => B): F[B] = F.map(ctx)(f)
    }
  }
}
