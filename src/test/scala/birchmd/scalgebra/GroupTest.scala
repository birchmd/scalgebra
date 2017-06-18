package birchmd.scalgebra

import org.scalatest.{FlatSpec, Matchers}
import scala.util.Random

class GroupTest extends FlatSpec with Matchers {
  "A group" should "have an inverse for every element" in {
    val group = Group.AdditiveIntegers
    import group.InfixOp

    val a: Int = Random.nextInt()
    (a op group.inverse(a)) should be (group.identity)
  }
}
