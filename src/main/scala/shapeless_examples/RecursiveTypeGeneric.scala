package shapeless_examples

import shapeless.{Generic, Lazy}
import shapeless_examples.CsvEncoder._

import scala.reflect.runtime.universe._

sealed trait Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case class Leaf[A](value: A) extends Tree[A]


object RecursiveTypeGeneric extends App {

  import CsvEncoder._
//  implicit val employeeGeneric = genericEncoder(Generic[Tree[Int]], Lazy[CsvEncoder[_]])
//      println(CsvEncoder[Branch[Int]])
   println(CsvEncoder[Leaf[Int]])
//  println(writeCsv(List(Branch[Int](Leaf(1), Leaf(2)))))
}

