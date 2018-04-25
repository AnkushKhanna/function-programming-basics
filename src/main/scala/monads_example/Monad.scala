package monads_example

import scala.language.higherKinds

trait Monad[F[_]] extends Functors[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](la: List[F[A]]): F[List[A]] =
    la.foldRight(unit(List[A]()))((fa, acc) =>
      flatMap(fa)(a =>
        map(acc)(ac =>
          a :: ac)
      )
    )

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la.map(a => f(a)))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def filterM[A](ls: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ls.foldLeft(unit(List[A]())) { (acc, a) =>
      flatMap(f(a)) { bool =>
        map(acc) { accValue =>
          if (bool) a :: accValue
          else accValue
        }
      }
    }
  }

}

object Monad {
  val op = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = None

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream.empty[A]

    override def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = fa.flatMap(f)
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List.empty[A]

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }
}