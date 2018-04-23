package monoid_example

import org.scalatest.FlatSpec

class ProductMonoidTest extends FlatSpec {
    "Bag" should "return correct values in sequence" in {

      val result = CombiningMonoids.bag(Vector("a", "b", "a", "c", "d", "b1", "c"))

      assert(result.get("a") === Some(2))
      assert(result.get("b") === Some(1))
      assert(result.get("c") === Some(2))
      assert(result.get("d") === Some(1))
      assert(result.get("b1") === Some(1))
    }
}
