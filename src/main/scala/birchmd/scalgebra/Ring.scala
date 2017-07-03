package birchmd.scalgebra

import birchmd.scalgebra.util.RussianPeasant

abstract class Ring[T] {
  def plus(a: T, b: T): T
  def times(a: T, b: T): T
  def addInv(a: T): T //additive inverse of a
  val zero: T //i.e. additive unit
  val one: T //i.e. multiplicative unit

  def pow(a: T, n: BigInt):T = RussianPeasant.rep[T](a, n, times, one)

  //infix notation for the operations
  implicit class ArithmeticOps(a: T) {
    def + (b: T): T = plus(a, b)
    def * (b: T): T = times(a, b)
    def - (b: T): T = plus(a, addInv(b))
    def ^ (n: BigInt): T = pow(a, n)
    def unary_- : T = addInv(a)
  }

  //Rings have an embedded additive group
  def additiveAbelianGroup: AbelianGroup[T] =  new AbelianGroup[T] {
    override def op(a: T, b: T): T = plus(a, b)
    override def inverse(a: T): T = addInv(a)
    override val identity: T = zero
  }

  //Rings have an embedded multiplicative monoid
  def multiplicativeMonoid: Monoid[T] = new Monoid[T] {
    override def op(a: T, b: T): T = times(a, b)
    override val identity: T = one
  }
}

object Ring {
  object DoubleRing extends Ring[Double] {
    override def plus(a: Double, b: Double): Double = a + b
    override def times(a: Double, b: Double): Double = a * b
    override def addInv(a: Double): Double = -a
    override val zero: Double = 0.0d
    override val one: Double = 1.0d
  }
  object LongRing extends Ring[Long] {
    override def plus(a: Long, b: Long): Long = a + b
    override def times(a: Long, b: Long): Long  = a * b
    override def addInv(a: Long): Long = -a
    override val zero: Long = 0L
    override val one: Long = 1L
  }
  object IntRing extends Ring[Int] {
    override def plus(a: Int, b: Int): Int = a + b
    override def times(a: Int, b: Int): Int  = a * b
    override def addInv(a: Int): Int = -a
    override val zero: Int = 0
    override val one: Int = 1
  }
  //ring based on modular arithmetic
  case class IntModM(m: Long) extends Ring[Long] {
    override def plus(a: Long, b: Long): Long = (a + b) % m
    override def times(a: Long, b: Long): Long  = (a * b) % m
    override def addInv(a: Long): Long = ((-a) % m + m) % m
    override val zero: Long = 0L
    override val one: Long = 1L
  }
}