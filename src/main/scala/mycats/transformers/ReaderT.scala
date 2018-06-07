package mycats.transformers

import scala.language.higherKinds
import mycats.categories.{Applicative, Functor, Monad}

final case class ReaderT[F[_], P, R](run: P => F[R]) {

  //val F: Monad[F] = Monad[F]

  def map[S](f: R => S)(implicit F: Functor[F]): ReaderT[F, P, S] =
    ReaderT{ p => F.map(run(p))(f) }

  def flatMap[S](f: R => ReaderT[F, P, S])(implicit F: Monad[F]): ReaderT[F, P, S] =
    ReaderT { p => F.flatMap(run(p)) { r => f(r).run(p) } }

    def andThen[S](that: ReaderT[F, R, S])(implicit F: Monad[F]): ReaderT[F, P, S] =
      ReaderT { p => F.flatMap(this.run(p))(r => that.run(r)) }

    def compose[O](that: ReaderT[F, O, P])(implicit F: Monad[F]): ReaderT[F, O, R] =
      //ReaderT { p => F.flatMap(that.run(p))(r => this.run(r)) }
      that andThen this
}

object ReaderT {

  def ask[F[_]: Applicative, A]: ReaderT[F, A, A] = ReaderT { Applicative[F].pure(_) }

  implicit def monad[F[_]: Monad, P]: Monad[ReaderT[F, P, ?]] = new Monad[ReaderT[F, P, ?]] {

    val F: Monad[F] = Monad[F]

    override def pure[R](r: R): ReaderT[F, P, R] =
      ReaderT(_ => F.pure(r))

    override def flatMap[R, S](fa: ReaderT[F, P, R])(f: R => ReaderT[F, P, S]): ReaderT[F, P, S] =
      fa flatMap f
  }
}
