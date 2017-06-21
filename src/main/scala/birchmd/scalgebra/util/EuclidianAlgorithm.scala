package birchmd.scalgebra.util

object EuclidianAlgorithm {
  val bigIntZero: BigInt = BigInt(0)

  @scala.annotation.tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  @scala.annotation.tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0L) a else gcd(b, a % b)

  @scala.annotation.tailrec
  def gcd(a: BigInt, b: BigInt): BigInt = if (b == bigIntZero) a else gcd(b, a % b)

  @scala.annotation.tailrec
  def extended(prev: (Int, Int, Int), curr: (Int, Int, Int)): (Int, Int, Int) = {
    val (r0, s0, t0) = prev
    val (r1, s1, t1) = curr

    if (r1 == 0) {
      prev
    } else {
      val q = r0 / r1
      val r2 = r0 - q * r1
      val s2 = s0 - q * s1
      val t2 = t0 - q * t1

      extended( curr, (r2, s2, t2) )
    }
  }
}
