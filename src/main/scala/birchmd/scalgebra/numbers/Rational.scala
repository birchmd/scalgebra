package birchmd.scalgebra.numbers

import birchmd.scalgebra.util.EuclidianAlgorithm.gcd
import scala.math.abs

//class has private constructor to force user to
//use the factory apply method which ensures the
//fraction is in reduced form with positive denominator
class Rational private (val num: Long, val denom: Long) {
  require(denom != 0, "Cannot have zero denominator!")

  def +(other: Rational): Rational = other match {
    case Rational(num2, denom2) => Rational((num * denom2) + (num2 * denom), denom * denom2)
  }
  def -(other: Rational): Rational = other match {
    case Rational(num2, denom2) => Rational((num * denom2) - (num2 * denom), denom * denom2)
  }
  def *(other: Rational): Rational = other match {
    case Rational(num2, denom2) => Rational(num * num2, denom * denom2)
  }
  def /(other: Rational): Rational = other match {
    case Rational(num2, denom2) => Rational(num * denom2, denom * num2)
  }
  def reciprocal: Rational = new Rational(denom, num)
  def negate: Rational = new Rational(-num, denom)

  override def toString: String = s"Rational($num, $denom)"

  override def equals(obj: scala.Any): Boolean = obj match {
    case Rational(num2, denom2) => num * denom2 == num2 * denom
    case somethingElse => super.equals(somethingElse)
  }
}

object Rational {
  def apply(a: Long, b: Long): Rational = {
    if (b < 0) {
      apply(-a, -b)
    } else {
      val d = gcd(abs(a), b)
      new Rational(a / d, b / d)
    }
  }

  def apply(a: Long): Rational = new Rational(a, 1)

  def unapply(arg: Rational): Option[(Long, Long)] = Some((arg.num, arg.denom))
}
