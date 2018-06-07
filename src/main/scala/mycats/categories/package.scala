package mycats

package object categories {

  type Id[A] = A

  object Id {

/*
    implicit def idFunctor: Functor[Id] = new Functor[Id] {
      override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
    }

    implicit def idApplicative: Applicative[Id] = new Applicative[Id] {
      override def pure[A](a: A): Id[A] = a
      override def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] = ff(fa)
    }

    implicit def idMonad: Monad[Id] = new Monad[Id] {
      override def pure[A](a: A): Id[A] = a
      override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
    }

    object syntax {

      implicit class FunctorId[A](o: Id[A]) {
        def map[B](f: A => B): Id[B] = Functor[Id].map(o)(f)
        def fmap[B](f: A => B): Id[B] = Functor[Id].map(o)(f)
      }
    }
*/
  }
}
