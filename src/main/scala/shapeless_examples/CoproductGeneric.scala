package shapeless_examples

import shapeless.{:+:, CNil, Coproduct, Inl, Inr}
import CsvEncoder.writeCsv

object CoproductGeneric extends App {
  def apply[A](implicit enc: CsvEncoder[A]) = enc

  def createEncoder[A](fun: A => List[String]): CsvEncoder[A] =
    (value: A) => fun(value)

  implicit val cnilEncoder: CsvEncoder[CNil] =
    createEncoder(cnil => throw new Exception)

  val shapes: List[Shape] = List(
    Rectangle(3.0, 4.0),
    Circle(1.0)
  )

  val shapesEncoded = writeCsv(shapes)
  println(shapesEncoded)
}
