package monads_example

import org.scalatest.FlatSpec

import scala.util.Try
import Monad._

class MonadTest extends FlatSpec {

//  import Monad._

  "Filter" should "filter correctly for List[Int] monad" in {

    val filteredList = listMonad.filterM(List(1, 2, 3, 4, 5, 6))(n => List(n % 2 != 0))
    assert(filteredList.size === 1)
    assert(filteredList.head === List(1, 3, 5))
  }

  "Monad Id" should "flatMap correctly" in {
    val a = Id("Hello ")
    val b = Id("World!")

    val ab = a flatMap (a =>
      b flatMap (b =>
        Id(a + b)
        )
      )

    val ab1 = for {
      a1 <- a
      b1 <- b
    } yield a1 + b1

    assert(ab1 === ab)
  }

}
