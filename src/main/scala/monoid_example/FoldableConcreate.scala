package monoid_example


object FoldableConcreate {

  val foldableList = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case Nil =>
          mb.zero
        case h :: Nil =>
          f(h)
        case list =>
          val (left, right) = list.splitAt(list.length / 2)
          mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
      }
  }

}
