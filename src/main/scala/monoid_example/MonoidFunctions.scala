package monoid_example

object MonoidFunctions {
  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))


  /**
    * Trying to use balanced fold
    **/
  def foldMapV[A, B](v: IndexedSeq[A])(f: A => B)(implicit m: Monoid[B]): B = {
    v match {
      case Seq() =>
        m.zero
      case seq if seq.length == 1 =>
        f(v.head)
      case seq =>
        val (left, right) = seq.splitAt(v.length / 2)
        m.op(foldMapV(left)(f), foldMapV(right)(f))
    }
  }
}
