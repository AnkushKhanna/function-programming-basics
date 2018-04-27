package monads_example

import org.scalatest.FlatSpec

import scala.util.Try

class MonadTest extends FlatSpec {

  import Monad._

  "Filter" should "filter correctly for List[Int] monad" in {

    val filteredList = listMonad.filterM(List(1, 2, 3, 4, 5, 6))(n => List(n % 2 != 0))
    assert(filteredList.size === 1)
    assert(filteredList.head === List(1, 3, 5))
  }

}
