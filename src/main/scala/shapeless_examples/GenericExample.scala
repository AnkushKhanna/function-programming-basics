package shapeless_examples

import shapeless.Generic

// OR type Coproduct in ADT
sealed trait Shape

// AND type Products in ADT
final case class Rectangle(width: Double, height: Double) extends Shape

final case class Circle(radius: Double) extends Shape

object GenericExample extends App {
  val gen = Generic[Shape]
  val r = gen.to(Rectangle(3.0, 4.0))
  println(r)
}

