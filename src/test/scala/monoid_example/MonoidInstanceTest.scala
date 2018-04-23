package monoid_example

import org.scalatest.FlatSpec

class MonoidInstanceTest extends FlatSpec {

  import Monoid.{booleanOr, stringMonoid}

  "FoldMapV" should "return correct concatenation" in {

    val v = IndexedSeq(1, 2, 3, 4, 5, 6)
    val result = MonoidFunctions.foldMapV[Int, String](v)(_.toString)

    val expectedResult = "123456"

    assert(result === expectedResult)
  }

  it should "return correct boolean type" in {

    val v = IndexedSeq(1, 0, 1, 0, 1, 1)
    val result = MonoidFunctions.foldMapV[Int, Boolean](v)((x) => if (x == 0) false else true)

    val expectedResult = true

    assert(result === expectedResult)
  }

}
