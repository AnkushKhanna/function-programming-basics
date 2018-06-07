package shapeless_examples

import shapeless.{:+:, ::, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {

  // summoner or materializer
  def apply[A](implicit enc: CsvEncoder[A]) = enc

  def createEncoder[A](fun: A => List[String]): CsvEncoder[A] =
    (value: A) => fun(value)

  implicit val doubleEncoder: CsvEncoder[Double] = createEncoder(d => List(d.toString))
  implicit val stringEncoder: CsvEncoder[String] = createEncoder(str => List(str))
  implicit val intEncoder: CsvEncoder[Int] = createEncoder(num => List(num.toString))
  implicit val booleanEncoder: CsvEncoder[Boolean] = createEncoder(bool =>
    List(if (bool) "yes" else "no")
  )
  implicit val hnilEncoder: CsvEncoder[HNil] = createEncoder(hnil => Nil)

  val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly


  implicit def hlistEncoder[H, T <: HList](implicit
                                           hEncoder: Lazy[CsvEncoder[H]],
                                           tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] =
    createEncoder {
      case h :: t =>
        hEncoder.value.encode(h) ++ tEncoder.encode(t)
    }

  implicit def coproductEncoder[H, T <: Coproduct](implicit
                                                   hEncoder: Lazy[CsvEncoder[H]],
                                                   tEncoder: CsvEncoder[T]): CsvEncoder[H :+: T] =
    createEncoder {
      case Inl(h) => hEncoder.value.encode(h)
      case Inr(t) => tEncoder.encode(t)
    }



  implicit def genericEncoder[A, R](implicit
                                    gen: Generic.Aux[A, R],
                                    enc: Lazy[CsvEncoder[R]]): CsvEncoder[A] =
    createEncoder(a => enc.value.encode(gen.to(a)))


  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
    values.map(v => enc.encode(v).mkString(",")).mkString("\n")

  // implicit resolution
  implicit def pairEncoder[A, B](implicit
                                 aEncoder: CsvEncoder[A],
                                 bEncoder: CsvEncoder[B]) =
    new CsvEncoder[(A, B)] {
      override def encode(pair: (A, B)): List[String] = {
        val (a, b) = pair
        aEncoder.encode(a) ++ bEncoder.encode(b)

      }
    }
}
