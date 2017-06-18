package birchmd.scalgebra

import org.scalatest.{FlatSpec, Matchers}
import scala.util.Random

//This unit test uses the additive integers monoid, but
//of course it should would for any valid monoid.
class MonoidTest extends FlatSpec with Matchers {
  "A monoid" should "have an associative operation" in  {
    import Monoid.implicits._
    implicit val monoid: Monoid[Int] = Monoid.AdditiveIntegers
    val a: Int = 1
    val b: Int = 2
    val c: Int = 3

    ((a op b) op c) should be (a op (b op c))
  }

  it should "have an identity element" in {
    import Monoid.implicits._
    implicit val monoid: Monoid[Int] = Monoid.AdditiveIntegers
    val a: Int = Random.nextInt()
    val b: Int = monoid.identity

    (a op b) should be (a)
  }
}
