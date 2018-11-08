package type_class_example

/*
* Type class is a class (group) of types, which satisfy some contract defined in a trait with addition that such
* functionality (trait and implementation) can be added without any changes to the original code.
* One could say that the same could be achieved by extending a simple trait,
* but with type classes it is not necessary to predict such a need beforehand.
* */
/*
* Type class is a class (group) of types, which satisfy some contract defined in a trait with addition that such
* functionality (trait and implementation) can be added without any changes to the original code.
* One could say that the same could be achieved by extending a simple trait,
* but with type classes it is not necessary to predict such a need beforehand.
* */
trait Show[A] {
  def show(a: A): String
}

object Show {

  def apply[A](implicit sh: Show[A]): Show[A] = sh

  //def show[A](a: A)(implicit sh: Show[A]) = sh.show(a)
  //def show[A: Show](a: A) = implicitly[Show[A]].show(a)
  object ops {
    def show[A: Show](a: A) = Show[A].show(a)

    final implicit class ShowOps[A](val a: A) extends AnyVal {
      def show(implicit sh: Show[A]) = sh.show(a)
    }

  }

  implicit val intCanShow: Show[Int] =
    (int: Int) => s"int $int"

  implicit val stringCanShow: Show[String] =
    (str: String) => s"string $str"
}

object Application extends  App {

  import Show.ops._

  println(show(20))
  println(20.show)
  println("show".show)

}
