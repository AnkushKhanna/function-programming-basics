package monoid_example


trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {
  implicit val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }


  implicit val intAdditionMonoid = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  implicit val intMultiplyMonoid = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  implicit val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  def optionMonoid[A](monoid: Monoid[A]): Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match {
      case (None, None) => None
      case (Some(a), Some(b)) => Some(monoid.op(a, b))
      case (Some(a), _) => a1
      case (_, Some(b)) => a2
    }

    override def zero: Option[A] = None
  }

}
