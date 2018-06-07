package shapeless_examples

import shapeless_examples.CsvEncoder._
import scala.reflect.runtime.universe._

sealed trait Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case class Leaf[A](value: A) extends Tree[A]


object RecursiveTypeGeneric extends App {

  //    println(CsvEncoder[Branch[Int]])
  // implicitly[CsvEncoder[Tree[Int]]]
}
