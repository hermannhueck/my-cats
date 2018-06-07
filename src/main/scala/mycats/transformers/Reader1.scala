package mycats.transformers

import mycats.categories.Monad

final case class Reader1[P, R](run: P => R) {

  def map[S](f: R => S): Reader1[P, S] =
    Reader1(run andThen f)

  def flatMap[S](f: R => Reader1[P, S]): Reader1[P, S] =
    Reader1 { p => f(run(p)).run(p) }

  def andThen[S](that: Reader1[R, S]): Reader1[P, S] =
    Reader1(this.run andThen that.run)

  def compose[O](that: Reader1[O, P]): Reader1[O, R] =
    // Reader1(that.run andThen this.run)
    that andThen this
}

object Reader1 {

  def ask[A]: Reader1[A, A] = Reader1(identity)

  implicit def monad[P]: Monad[Reader1[P, ?]] = new Monad[Reader1[P, ?]] {

    override def pure[R](r: R): Reader1[P, R] =
      Reader1(_ => r)

    override def flatMap[R, S](fa: Reader1[P, R])(f: R => Reader1[P, S]): Reader1[P, S] =
      fa flatMap f
  }
}
