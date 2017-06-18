package birchmd.scalgebra

import birchmd.scalgebra.util.RussianPeasant

trait Monoid[T] { self =>
  def op (a: T, b: T): T //must be associative
  val identity: T //must have the property (x: T) => op(x, unit) == x

  implicit class InfixOp(a: T) {
    def op (b: T): T = self.op(a, b)
  }
  def pow(base: T, exponent: BigInt): T = {
    RussianPeasant.rep[T](base, exponent, op, identity)
  }
}

object Monoid {
  object AdditiveIntegers extends Monoid[Int] {
    override def op(a: Int, b: Int): Int = a + b
    override val identity: Int = 0
  }
}
