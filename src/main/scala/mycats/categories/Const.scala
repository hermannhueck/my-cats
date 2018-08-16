package mycats.categories

case class Const[A, B](getConst: A)

object Const {

  implicit def monoidApplicative[M](implicit M: Monoid[M]): Applicative[Const[M, ?]] = new Applicative[Const[M, ?]] {

    override def pure[A](a: A): Const[M, A] = Const(Monoid[M].empty)
    override def ap[A, B](ff: Const[M, A => B])(fa: Const[M, A]): Const[M, B] = Const(M.combine(ff.getConst, fa.getConst))
  }

}