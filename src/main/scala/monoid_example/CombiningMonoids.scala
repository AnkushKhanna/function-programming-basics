package monoid_example

object CombiningMonoids {

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (a.op(a1._1, a2._1), b.op(a1._2, a2._2))

    override def zero: (A, B) = (a.zero, b.zero)
  }

  def mapMergeMonoid[K, V](v: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def zero: Map[K, V] = Map()

    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
      (a1.keySet ++ a2.keySet)
        .foldLeft(zero)((acc, k) => acc.updated(k, v.op(a1.getOrElse(k, v.zero), a2.getOrElse(k, v.zero))))
  }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def zero: A => B = _ => b.zero

    override def op(a1: A => B, a2: A => B): A => B = a => b.op(a1(a), a2(a))
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val m: Monoid[Map[A, Int]] = mapMergeMonoid(Monoid.intAdditionMonoid)
    as.map(x => Map(x -> 1)).foldLeft(m.zero)(m.op)
  }

}
