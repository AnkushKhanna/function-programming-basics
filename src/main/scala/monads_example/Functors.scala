package monads_example

import scala.language.higherKinds

trait Functors[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distributed[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistrubute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
}


