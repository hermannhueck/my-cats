package mycats.transformers

import mycats.categories.{Functor, Monad}

import scala.language.higherKinds

final case class EitherT[F[_], L, R](value: F[Either[L, R]]) {

  def map[S](f: R => S)(implicit F: Functor[F]): EitherT[F, L, S] =
    EitherT(F.map(value)(e => e.map(f)))

  def flatMap[S](f: R => EitherT[F, L, S])(implicit F: Monad[F]): EitherT[F, L, S] =
    EitherT(
      F.flatMap(value)(e => e.fold(
        l => F.pure(Left(l)),
        r => f(r).value))
    )

  // combines 2 eithers using Semigroups for L and R
  def +++(other: EitherT[F, L, R]): EitherT[F, L, R] = ???
}

object EitherT {

//  typecalss instance ambiguous with the monad typeclass ???
//  implicit def functor[F[_]: Functor, L]: Functor[EitherT[F, L, ?]] = new Functor[EitherT[F, L, ?]] {

//    override def map[R, S](fa: EitherT[F, L, R])(f: R => S): EitherT[F, L, S] = fa map f
//  }

  implicit def monad[F[_]: Monad, L]: Monad[EitherT[F, L, ?]] = new Monad[EitherT[F, L, ?]] {

    val F: Monad[F] = implicitly[Monad[F]]

    override def pure[R](r: R): EitherT[F, L, R] = EitherT[F, L, R](F.pure(Right(r)))

    override def flatMap[R, S](fa: EitherT[F, L, R])(f: R => EitherT[F, L, S]): EitherT[F, L, S] = fa flatMap f

    override def map[R, S](fa: EitherT[F, L, R])(f: R => S): EitherT[F, L, S] = fa map f
  }
}
