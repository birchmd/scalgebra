package birchmd.scalgebra

trait Ring[T] extends AbelianGroup[T] {
  def plus(a: T, b: T): T
  def times(a: T, b: T): T
  def negate(a: T): T //additive inverse of a
  val zero: T //i.e. additive unit
  val one: T //i.e. multiplicative unit

  //infix notation for the operations
  implicit class InfixOps(a: T) {
    def + (b: T): T = plus(a, b)
    def * (b: T): T = times(a, b)
  }

  //Rings have an embedded additive group
  override def op(a: T, b: T): T = plus(a, b)
  override def inverse(a: T): T = negate(a)
  override val identity: T = zero

  //Rings havce an embedded multiplicative monoid
  def multiplicativeMonoid: Monoid[T] = new Monoid[T] {
    override def op(a: T, b: T): T = times(a, b)
    override val identity: T = one
  }
}

object Ring {
  object DoubleRing extends Ring[Double] {
    override def plus(a: Double, b: Double): Double = a + b
    override def times(a: Double, b: Double): Double = a * b
    override def negate(a: Double): Double = -a
    override val zero: Double = 0.0d
    override val one: Double = 1.0d
  }
  object LongRing extends Ring[Long] {
    override def plus(a: Long, b: Long): Long = a + b
    override def times(a: Long, b: Long): Long  = a * b
    override def negate(a: Long): Long = -a
    override val zero: Long = 0L
    override val one: Long = 1L
  }
  object IntRing extends Ring[Int] {
    override def plus(a: Int, b: Int): Int = a + b
    override def times(a: Int, b: Int): Int  = a * b
    override def negate(a: Int): Int = -a
    override val zero: Int = 0
    override val one: Int = 1
  }
}