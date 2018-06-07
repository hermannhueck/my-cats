package mycats.transformers

import mycats.categories.{Id, Monad}

object Reader {

  def apply[P, R](run: P => R): Reader[P, R] = ReaderT[Id, P, R](run)

  def ask[A]: Reader[A, A] = Reader(identity)

  implicit def monad[P]: Monad[Reader[P, ?]] = new Monad[Reader[P, ?]] {

    override def pure[R](r: R): Reader[P, R] =
      Reader(_ => r)

    override def flatMap[R, S](fa: Reader[P, R])(f: R => Reader[P, S]): Reader[P, S] =
      fa flatMap f
  }
}
