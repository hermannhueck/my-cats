package mycats.categories

import scala.concurrent.Future
import scala.language.higherKinds

// typeclass functor
trait Functor[F[_]] extends Any { self =>

  // intrinsic abstract Functor method

  def map[A, B](fa: F[A])(f: A => B): F[B]


  // method implementations in terms of map

  def fmap[A, B](fa: F[A])(f: A => B): F[B] = map(fa)(f) // alias for map

  def lift[A, B](f: A => B): F[A] => F[B] = fa => map(fa)(f)

  def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)

  def void[A](fa: F[A]): F[Unit] = as(fa, ())

/*
  def compose[G[_]: Functor]: Functor[Lambda[X => F[G[X]]]] = new Functor[Lambda[X => F[G[X]]]] {
    override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
      self.map(fga)(ga => implicitly[Functor[G]].map(ga)(f))
  }
*/

  def compose[G[_]: Functor]: Functor[Lambda[X => F[G[X]]]] =
    new Functor.Composite[F, G] {
      def F: Functor[F] = self
      def G: Functor[G] = Functor[G]
    }
}

object Functor {

  trait Composite[F[_], G[_]] extends Any with Functor[Lambda[X => F[G[X]]]] {
    def F: Functor[F]
    def G: Functor[G]
    override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] =
      F.map(fa)(G.lift(f))
  }

  def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]

  // default typeclass instances in implicit scope

  implicit def listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit def optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit def futureFunctor: Functor[Future] = new Functor[Future] {
    import scala.concurrent.ExecutionContext.Implicits.global
    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  }

  implicit def idFunctor: Functor[Id] = new Functor[Id] {
    override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
  }

  implicit def eitherFunctor[L]: Functor[Either[L, ?]] = new Functor[Either[L, ?]] {
    override def map[R1, R2](fa: Either[L, R1])(f: R1 => R2): Either[L, R2] = fa map f
  }

  implicit def pairFunctor[L]: Functor[Tuple2[L, ?]] = new Functor[Tuple2[L, ?]] {
    override def map[R1, R2](fa: Tuple2[L, R1])(f: R1 => R2): Tuple2[L, R2] = fa match {
      case(x, y) => (x, f(y))
    }
  }

  implicit def function1Functor[PARAM]: Functor[Function1[PARAM, ?]] = new Functor[Function1[PARAM, ?]] {
    override def map[RESULT1, RESULT2](fa: Function1[PARAM, RESULT1])(f: RESULT1 => RESULT2): Function1[PARAM, RESULT2] = fa andThen f
  }

  object syntax {

    implicit class FunctorList[A](l: List[A]) {
      def fmap[B](f: A => B): List[B] = Functor[List].fmap(l)(f)
    }

    implicit class FunctorOption[A](o: Option[A]) {
      def fmap[B](f: A => B): Option[B] = Functor[Option].fmap(o)(f)
    }

    implicit class FunctorFuture[A](fut: Future[A]) {
      def fmap[B](f: A => B): Future[B] = Functor[Future].fmap(fut)(f)
    }

    implicit class FunctorId[A](o: Id[A]) {
      def fmap[B](f: A => B): Id[B] = Functor[Id].fmap(o)(f)
    }

    implicit class FunctorEither[L, R1](e: Either[L, R1]) {
      def fmap[R2](f: R1 => R2): Either[L, R2] = Functor[Either[L, ?]].fmap(e)(f)
    }

    implicit class FunctorPair[L, R1](l: Tuple2[L, R1]) {
      def fmap[R2](f: R1 => R2): Tuple2[L, R2] = Functor[Tuple2[L, ?]].fmap(l)(f)
    }

    implicit class FunctorFunction1[PARAM, RESULT1](g: Function1[PARAM, RESULT1]) {
      def fmap[RESULT2](f: RESULT1 => RESULT2): Function1[PARAM, RESULT2] = Functor[Function1[PARAM, ?]].fmap(g)(f)
    }
  }

  object syntax2 {

    // type enrichments for functors with one type parameter

    abstract class FunctorF1[F1[_]: Functor, A](fa: F1[A]) {
      def fmap[B](f: A => B): F1[B] = Functor[F1].fmap(fa)(f)
    }

    implicit class FunctorList[A](l: List[A]) extends FunctorF1[List, A](l)

    implicit class FunctorOption[A](o: Option[A]) extends FunctorF1[Option, A](o)

    implicit class FunctorFuture[A](fut: Future[A]) extends FunctorF1[Future, A](fut)

    // type enrichments for functors with two type parameters

    abstract class FunctorF2[F2[_, _], X, A](fa: F2[X, A])(implicit F: Functor[F2[X, ?]]) {
      def fmap[B](f: A => B): F2[X, B] = F.fmap(fa)(f)
    }

    implicit class FunctorEither[LEFT, RIGHT](e: Either[LEFT, RIGHT]) extends FunctorF2[Either, LEFT, RIGHT](e)

    implicit class FunctorPair[LEFT, RIGHT](e: Tuple2[LEFT, RIGHT]) extends FunctorF2[Tuple2, LEFT, RIGHT](e)

    implicit class FunctorFunction[PARAM, RESULT](f: Function1[PARAM, RESULT]) extends FunctorF2[Function1, PARAM, RESULT](f)
  }
}
