package mycats.categories

import scala.language.higherKinds

trait Foldable[F[_]] extends Any { self =>

  // intrinsic abstract Foldable methods

  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B

  def foldRight[A, B](fa: F[A], b: B)(f: (A, B) => B): B


  // method implementations in terms of foldLeft or foldRight

  def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B = {
    val mb = implicitly[Monoid[B]]
    foldRight(fa, mb.empty)((a, b) => mb.combine(f(a), b))
    // same as: foldLeft(fa, mb.empty)((b, a) => mb.combine(b, f(a)))
  }

  def fold[A: Monoid](fa: F[A]): A = foldMap(fa)(identity)

  def combineAll[A: Monoid](fa: F[A]): A = fold(fa)

  def exists[A](fa: F[A])(p: A => Boolean): Boolean =
    foldRight(fa, false)((a, b) => p(a) || b)

  def contains[A](fa: F[A])(elem: A): Boolean = exists(fa)(_ == elem)

  def forall[A](fa: F[A])(p: (A) => Boolean): Boolean = !exists(fa)(!p(_))

  def find[A](fa: F[A])(p: A => Boolean): Option[A] =
    foldRight(fa, Option.empty[A]) { (a, opt) =>
      if (opt.isDefined) opt
      else if (p(a)) Option(a)
      else Option.empty
    }

  def toList[A](fa: F[A]): List[A] =
    foldRight(fa, List.empty[A]) { (a, as) => a :: as }

  def filterToList[A](fa: F[A])(p: A => Boolean): List[A] =
    foldRight(fa, List.empty[A]) { (a, as) =>
      if (p(a)) a :: as else as
    }

  def mapToList[A, B](fa: F[A])(f: A => B): List[B] =
    foldRight(fa, List.empty[B]) { (a, bs) => f(a) :: bs }

  def flatMapToList[A, B](fa: F[A])(f: A => List[B]): List[B] =
    foldRight(fa, List.empty[B]) { (a, bs) => f(a) ++ bs }

  def length[A](fa: F[A]): Long = foldRight(fa, 0L)((a, acc) => acc + 1L)

  def size[A](fa: F[A]): Long = length(fa)

  def isEmpty[A](fa: F[A]): Boolean = length(fa) == 0

  def nonEmpty[A](fa: F[A]): Boolean = !isEmpty(fa)

  def get[A](fa: F[A])(idx: Long): Option[A] =
    foldRight(fa, (0L, Option.empty[A])) { (a, acc) => {
        val (i, opt) = acc
        if (i == idx) (i + 1, Some(a)) else (i + 1, opt)
      }
    }._2

  def sum[A: Numeric](fa: F[A]): A = {
    val num = implicitly[Numeric[A]]
    foldRight(fa, num.zero)(num.plus)
  }

  def compose[G[_]: Foldable]: Foldable[Lambda[X => F[G[X]]]] =
    new Foldable.Composite[F, G] {
      def F: Foldable[F] = self
      def G: Foldable[G] = Foldable[G]
    }
}

object Foldable {

  trait Composite[F[_], G[_]] extends Any with Foldable[Lambda[X => F[G[X]]]] {
    def F: Foldable[F]
    def G: Foldable[G]
    def foldLeft[A, B](fa: F[G[A]], initial: B)(f: (B, A) => B): B =
      F.foldLeft(fa, initial)((acc, ga) => G.foldLeft(ga, acc)(f))
    def foldRight[A, B](fa: F[G[A]], initial: B)(f: (A, B) => B): B =
      F.foldRight(fa, initial)((ga, acc) => G.foldRight(ga, acc)(f))
  }


  def apply[F[_]: Foldable]: Foldable[F] = implicitly[Foldable[F]]

  implicit def listFoldable: Foldable[List] = new Foldable[List] {

    override def foldLeft[A, B](fa: List[A], acc: B)(op: (B, A) => B): B = fa match {
      case Nil => acc
      case h :: t => op(t.foldLeft(acc)(op), h)
    }

    override def foldRight[A, B](fa: List[A], acc: B)(op: (A, B) => B): B = fa match {
      case Nil => acc
      case h :: t => op(h, t.foldRight(acc)(op))
    }
  }

  implicit def optionFoldable: Foldable[Option] = new Foldable[Option] {

    override def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa match {
      case None => b
      case Some(a) => f(b, a)
    }

    override def foldRight[A, B](fa: Option[A], b: B)(f: (A, B) => B): B = fa match {
      case None => b
      case Some(a) => f(a, b)
    }
  }
}
