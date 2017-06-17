package birchmd.scalgebra

trait Ring[T] {
  def plus(a: T, b: T): T
  def times(a: T, b: T): T
  val zero: T //i.e. additive unit
  val one: T //i.e. multiplicative unit
}

object Ring {
  object DoubleRing extends Ring[Double] {
    override def plus(a: Double, b: Double): Double = a + b
    override def times(a: Double, b: Double): Double = a * b
    override val zero: Double = 0.0d
    override val one: Double = 1.0d
  }
  object LongRing extends Ring[Long] {
    override def plus(a: Long, b: Long): Long = a + b
    override def times(a: Long, b: Long): Long  = a * b
    override val zero: Long = 0L
    override val one: Long = 1L
  }
  object IntRing extends Ring[Int] {
    override def plus(a: Int, b: Int): Int = a + b
    override def times(a: Int, b: Int): Int  = a * b
    override val zero: Int = 0
    override val one: Int = 1
  }
}