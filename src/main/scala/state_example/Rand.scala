package state_example

object RandImplicit {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = rng => rng.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)


  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }


  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

  //Implementation

  def nonNegativeInt: Rand[Int] = map(_.nextInt) { i => if (i < 0) -i + 1 else i }


  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def double: Rand[Double] = map(_.nextInt)(i => i.toDouble / i)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

}

trait RNG {
  def nextInt: (Int, RNG)

  def double: (Double, RNG) = {
    val (i, rng) = this.nextInt
    (i.toDouble / i, rng)
  }

  def intDouble = {
    val (i, newRNG) = this.nextInt
    val (d, newerRNG) = newRNG.double
    ((i, d), newerRNG)
  }


  def doubleInt = {
    val ((i, d), newRNG) = this.intDouble
    ((d, i), newRNG)
  }

  def ints(count: Int): (List[Int], RNG) = count match {
    case 0 => (Nil, this)
    case c =>
      val (i, rng) = this.nextInt
      val (list, newRNG) = rng.ints(c - 1)
      (i :: list, newRNG)
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
