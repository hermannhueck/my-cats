package mycats.categories

import scala.concurrent.Future
import scala.language.higherKinds

trait Applicative[F[_]] extends Any with Functor[F] { self =>

  // intrinsic abstract Applicative methods

  def pure[A](a: A): F[A]

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]


  // method implementations in terms of pure and ap

  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)

  def <*>[A, B](ff: F[A => B])(fa: F[A]): F[B] = ap(ff)(fa) // alias for ap

  def ap2[A, B, Z](ff: F[(A, B) => Z])(fa: F[A], fb: F[B]): F[Z] = {
    val ffBZ: F[B => Z] = ap(map(ff)(f => (a:A) => (b:B) => f(a, b)))(fa)
    ap(ffBZ)(fb)
  }

  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) ⇒ Z): F[Z] =
    ap(map(fa)(a => f(a, _: B)))(fb) // same as: ap(map(fb)(b => f((_: A), b)))(fa)

  def map3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) ⇒ Z): F[Z] =
    ap(map2(fa, fb)((a, b) => (c:C) => f(a, b, c)))(fc)

  def map4[A, B, C, D, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) ⇒ Z): F[Z] =
    map2(tuple2(fa, fb), tuple2(fc, fd)) {case ((a, b), (c, d)) => f(a, b, c, d)}
    // same as: ap(map3(fa, fb, fc)((a, b, c) => (d:D) => f(a, b, c, d)))(fd)

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def tuple2[A, B](fa: F[A], fb: F[B]): F[(A, B)] = product(fa, fb)

  def tuple3[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] = map3(fa, fb, fc)((_, _, _))

  def flip[A, B](ff: F[A => B]): F[A] => F[B] = fa => ap(ff)(fa)

/*
  def compose[G[_] : Applicative]: Applicative[Lambda[X => F[G[X]]]] = new Applicative[Lambda[X => F[G[X]]]] {
    override def pure[A](a: A): F[G[A]] = self.pure(Applicative[G].pure(a))
    override def ap[A, B](ff: F[G[A => B]])(fga: F[G[A]]): F[G[B]] = {
      val fga2gb: F[G[A] => G[B]] = self.map(ff)(gab => Applicative[G].flip(gab))
      self.ap(fga2gb)(fga)
    }
  }
*/

  def compose[G[_]: Applicative]: Applicative[Lambda[X => F[G[X]]]] =
    new Applicative.Composite[F, G] {
      def F: Applicative[F] = self
      def G: Applicative[G] = Applicative[G]
    }
}

object Applicative {

  trait Composite[F[_], G[_]] extends Any with Applicative[Lambda[X => F[G[X]]]] {
    def F: Applicative[F]
    def G: Applicative[G]
    def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))
    override def ap[A, B](f: F[G[A => B]])(fa: F[G[A]]): F[G[B]] = {
      val flipped: F[G[A] => G[B]] = F.map(f)(G.flip)
      F.ap(flipped)(fa)
    }
  }

  def apply[F[_]: Applicative]: Applicative[F] = implicitly[Applicative[F]]

  // default typeclass instances in implicit scope

  implicit def listApplicative: Applicative[List] = new Applicative[List] {
    override def pure[A](a: A): List[A] = List(a)
    override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
      for {f <- ff; a <- fa} yield f(a)
  }

  implicit def optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](a: A): Option[A] = Some(a)
    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = (ff, fa) match {
      case (Some(f), Some(a)) => Option(f(a))
      case (_, _) => Option.empty
    }
  }

  implicit def futureApplicative: Applicative[Future] = new Applicative[Future] {
    import scala.concurrent.ExecutionContext.Implicits.global
    override def pure[A](a: A): Future[A] = Future.successful(a)
    override def ap[A, B](ff: Future[A => B])(fa: Future[A]): Future[B] =
      for {f <- ff; a <- fa} yield f(a)
  }

  implicit def idApplicative: Applicative[Id] = new Applicative[Id] {
    override def pure[A](a: A): Id[A] = a
    override def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] = ff(fa)
  }

  implicit def eitherApplicative[L: Monoid]: Applicative[Either[L, ?]] = new Applicative[Either[L, ?]] {
    override def pure[R](r: R): Either[L, R] = Right(r)
    override def ap[R1, R2](ff: Either[L, R1 => R2])(fa: Either[L, R1]): Either[L, R2] = (ff, fa) match {
      case (Right(f), Right(a)) => Right(f(a))
      case (Left(l1), Left(l2)) => Left(Monoid[L].combine(l1, l2))
      case (Left(l1), _) => Left(l1)
      case (_, Left(l2)) => Left(l2)
    }
  }

  implicit def tuple2Applicative[L: Monoid]: Applicative[Tuple2[L, ?]] = new Applicative[Tuple2[L, ?]] {
    override def pure[R](r: R): (L, R) = (Monoid[L].empty, r)
    override def ap[R1, R2](ff: (L, R1 => R2))(fa: (L, R1)): (L, R2) = (ff, fa) match {
      case ((lf, rf), (la, ra)) => (Monoid[L].combine(lf, la), rf(ra))
    }
  }

  implicit def function1Applicative[P]: Applicative[Function1[P, ?]] = new Applicative[Function1[P, ?]] {

    import Applicative.ops

    override def pure[R](r: R): P => R = _ => r

    /*
    override def ap[R1, R2](ff: P => R1 => R2)(fa: P => R1): P => R2 = { p =>
      println(s"----- p = $p")
      val faResult: R1 = fa(p)
      println(s"----- faResult = $faResult")
      val f: R1 => R2 = ff(p)
      println(s"----- f = $f")
      val fResult = f(faResult)
      println(s"----- fResult = $fResult")
      fResult
    }
    */
    override def ap[R1, R2](ff: P => R1 => R2)(fa: P => R1): P => R2 = { p =>
      ff(p)(fa(p))
      //map(ff)(f => f(fa(p)))(p)
    }
  }

  object ops {

    implicit class ApplicativeF[F[_]: Applicative, A](ctx: F[A]) {

      private val F = Applicative[F]

      def ap[B](ff: F[A => B]): F[B] = F.ap(ff)(ctx)
      def <*>[B](ff: F[A => B]): F[B] = ap(ff)

      def map[B](f: A => B): F[B] = F.map(ctx)(f)
      def fmap[B](f: A => B): F[B] = map(f)
      def <|>[B](f: A => B): F[B] = map(f) // <$> is not allowed
    }
  }
}
