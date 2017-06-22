package birchmd.scalgebra.numbers

import birchmd.scalgebra.Ring

case class Complex[T](re: T, im: T)(implicit ring: Ring[T]) {
  import ring.ArithmeticOps

  def +(other: Complex[T]): Complex[T] = other match {
    case Complex(re2, im2) => Complex(re + re2, im + im2)
  }
  def *(other: Complex[T]): Complex[T] = other match {
    case Complex(re2, im2) => Complex( (re * re2) - (im * im2), (re * im2) + (re2 * im) )
  }
  def unary_- : Complex[T] = Complex(-re, -im)
  def conj: Complex[T] = Complex(re, -im)
}

object Complex {
  def extendRing[T](ring: Ring[T]): Ring[Complex[T]] = new Ring[Complex[T]] {
    override def addInv(a: Complex[T]): Complex[T] = -a
    override def plus(a: Complex[T], b: Complex[T]): Complex[T] = a + b
    override def times(a: Complex[T], b: Complex[T]): Complex[T] = a * b
    override val one: Complex[T] = Complex(ring.one, ring.zero)(ring)
    override val zero: Complex[T] = Complex(ring.zero, ring.zero)(ring)
  }
}
