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
}
