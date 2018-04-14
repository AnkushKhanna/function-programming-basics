package state_example

import org.scalatest.FlatSpec

class RNGTest extends FlatSpec {

  import RandImplicit._

  val rng = SimpleRNG(100)
  "NonNegativeInt" should "return different int" in {
    def start(i: Int, randomNG: RNG): Unit = i match {
      case 100 => assert(true)
      case _ =>
        val (r, newRNG) = nonNegativeInt(randomNG)
        assert(r > 0)
        start(i + 1, newRNG)
    }

    start(0, rng)
  }

  "Double" should "return double between 0 and 1" in {
    def start(i: Int, randomNG: RNG): Unit = i match {
      case 100 => assert(true)
      case _ =>
        val (r, newRNG) = double(rng)
        assert(r > 0.0 && r <= 1.0)
        start(i + 1, newRNG)
    }

    start(0, rng)
  }

  "Ints" should "return correct list" in {
    val (l, newRNG) = rng.ints(10)

    assert(l.length === 10)
    assert(newRNG != rng)
  }

  "Rand example both" should "correct things" in {
    val ((i, d), _) = both(int, double)(rng)
    assert(i.isInstanceOf[Int])
    assert(d.isInstanceOf[Double])

  }
}

