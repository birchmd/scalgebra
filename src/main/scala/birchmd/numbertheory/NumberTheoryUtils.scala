package birchmd.numbertheory

object NumberTheoryUtils {

  val bigIntZero: BigInt = BigInt(0)

  @scala.annotation.tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  @scala.annotation.tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0L) a else gcd(b, a % b)

  @scala.annotation.tailrec
  def gcd(a: BigInt, b: BigInt): BigInt = if (b == bigIntZero) a else gcd(b, a % b)


  @scala.annotation.tailrec
  def extEuclid(prev: (Int, Int, Int), curr: (Int, Int, Int)): (Int, Int, Int) = {
    val (r0, s0, t0) = prev
    val (r1, s1, t1) = curr

    if (r1 == 0) {
      prev
    } else {
      val q = r0 / r1
      val r2 = r0 - q * r1
      val s2 = s0 - q * s1
      val t2 = t0 - q * t1

      extEuclid( curr, (r2, s2, t2) )
    }
  }

  def solveLinearDiophantine(a: Int, b: Int, c: Int): (Int, Int) = {
    val (g, s, t) = extEuclid( (a, 1, 0), (b, 0, 1) )
    (s * (c / g), t * (c / g))
  }


  //computes the multiplicative inverse of n mod m
  def modularArithmeticInverse(n: Int, m: Long): Long = {

    @scala.annotation.tailrec
    def work(r: Long, newR: Long, t: Long, newT: Long): Long = {
      if (newR == 0) {
        if (t < 0) t + m else t
      } else {
        val q = r / newR
        work(newR, r - (q * newR), newT, t - (q * newT))
      }
    }

    work(m, n, 0, 1)
  }
}
