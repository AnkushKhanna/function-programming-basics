package monads_example

object ConcreteFunctors {

  val listFuncton = new Functors[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f
  }

}
