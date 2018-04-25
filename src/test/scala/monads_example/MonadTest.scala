package monads_example

import org.scalatest.FlatSpec

class MonadTest extends FlatSpec {

  import Monad._

  "Filter" should "filter correctly for List[Int] monad" in {
    val filteredList = op.filterM(List(1, 2, 3, 4, 5, 6))(i => Option(i % 2 == 0))
    assert(filteredList.size === 3)
  }

}
