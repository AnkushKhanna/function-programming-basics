package shapeless_examples

import shapeless.Generic

case class Employee(name: String, number: Int, manager: Boolean)

case class IceCream(name: String, numCherries: Int, inCone: Boolean)


object Test extends App {

  import CsvEncoder._

  val employees = List(
    Employee("ABC", 1, true),
    Employee("XYZ", 2, false)
  )

  val iceCreams = List(
    IceCream("Chocolate", 1, true),
    IceCream("Vanilla", 2, false)
  )

  implicit val iceCreamGeneric = genericEncoder(Generic[IceCream], reprEncoder)
  val iceEncoder = writeCsv(iceCreams)
  println(iceEncoder)

  implicit val employeeGeneric = genericEncoder(Generic[Employee], reprEncoder)
  val employeeEncoder = writeCsv(employees)
  println(employeeEncoder)

  val zipEncoder = writeCsv(iceCreams zip employees)
  println(zipEncoder)
}




