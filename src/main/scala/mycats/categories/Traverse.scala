package mycats.categories

import scala.language.higherKinds

trait Traverse[F[_]] extends Any with Foldable[F] with Functor[F] { self =>

  def traverse[G[_], A, B](fa: F[A])(f: (A) ⇒ G[B])(implicit AG: Applicative[G]): G[F[B]]

  def sequence[G[_], A](fga: F[G[A]])(implicit AG: Applicative[G]): G[F[A]] =
    traverse(fga)(identity)

  def compose[G[_]: Traverse]: Traverse[Lambda[X => F[G[X]]]] =
    new Traverse.Composite[F, G] {
      def F: Traverse[F] = self
      def G: Traverse[G] = Traverse[G]
    }
}

object Traverse {

  trait Composite[F[_], G[_]] extends Any with Traverse[Lambda[X => F[G[X]]]] with Functor.Composite[F, G] with Foldable.Composite[F, G] {
    def F: Traverse[F]

    def G: Traverse[G]

    def traverse[H[_] : Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
      F.traverse(fga)(ga => G.traverse(ga)(f))
  }

  def apply[F[_]: Traverse]: Traverse[F] = implicitly[Traverse[F]]

  implicit def listTraverse: Traverse[List] = new Traverse[List] {

    override def traverse[F[_], A, B](fa: List[A])(f: A => F[B])(implicit AF: Applicative[F]): F[List[B]] = {
/*
      fa.foldLeft(AF.pure(List.empty[B])) { (acc, elem) =>
        AF.map2(acc, f(elem))(_ :+ _)
      }
*/
      fa.foldRight(AF.pure(List.empty[B])) { (elem, acc) => // foldRight impl is more efficient for Lists
        AF.map2(f(elem), acc)(_ :: _)
      }
    }

    override def foldLeft[A, B](fa: List[A], acc: B)(op: (B, A) => B): B = fa match {
      case Nil => acc
      case h :: t => op(t.foldLeft(acc)(op), h)
    }

    override def foldRight[A, B](fa: List[A], acc: B)(op: (A, B) => B): B = fa match {
      case Nil => acc
      case h :: t => op(h, t.foldRight(acc)(op))
    }

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f
  }

  implicit def optionTraverse: Traverse[Option] = new Traverse[Option] {

    override def traverse[F[_], A, B](fa: Option[A])(f: A => F[B])(implicit AF: Applicative[F]): F[Option[B]] = fa match {
      case None => AF.pure(Option.empty[B])
      case Some(a) => AF.map(f(a))(Some(_))
    }

    override def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa match {
      case None => b
      case Some(a) => f(b, a)
    }

    override def foldRight[A, B](fa: Option[A], b: B)(f: (A, B) => B): B = fa match {
      case None => b
      case Some(a) => f(a, b)
    }

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
  }
}

