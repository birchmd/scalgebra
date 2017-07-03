package birchmd.scalgebra

import birchmd.scalgebra.numbers.Rational

abstract class Field[T] extends Ring[T] {
  def multInv(a: T): T // multiplicative inverse

  def multiplicativeAbelianGroup: AbelianGroup[T] = new AbelianGroup[T] {
    override def inverse(a: T): T = multInv(a)
    override def op(a: T, b: T): T = times(a, b)
    override val identity: T = one
  }
}

object Field {
  object RationalField extends Field[Rational] {
    override def plus(a: Rational, b: Rational): Rational = a + b
    override def times(a: Rational, b: Rational): Rational = a * b
    override def addInv(a: Rational): Rational = a.negate
    override def multInv(a: Rational): Rational = a.reciprocal
    override val one: Rational = Rational(1)
    override val zero: Rational = Rational(0)
  }

  //Finite Field of integers mod p, where p is a prime.
  //Values are on the interval [0, p - 1].
  case class IntModPField(p: Long) extends Field[Long] {
    override def plus(a: Long, b: Long): Long = (a + b) % p
    override def times(a: Long, b: Long): Long = (a * b) % p
    override def addInv(a: Long): Long = (((-a) % p) + p) % p //add p at the end to get positive answer
    override def multInv(a: Long): Long = {
      @scala.annotation.tailrec
      def work(r: Long, newR: Long, t: Long, newT: Long): Long = {
        if (newR == 0) {
          if (t < 0) t + p else t
        } else {
          val q = r / newR
          work(newR, r - (q * newR), newT, t - (q * newT))
        }
      }

      work(p, a, 0, 1)
    }
    override val one: Long = 1L
    override val zero: Long = 0L
  }
}
