package birchmd.scalgebra.linalg

import birchmd.scalgebra.Ring
import org.scalatest.{FlatSpec, Matchers}

class MatrixTest extends FlatSpec with Matchers {
  "A matrix" should "add element-wise" in {
    implicit val ring = Ring.IntRing
    val a = Matrix[Int](2, 2, Vector(1, 2, 3, 4))
    val b = Matrix[Int](2, 2, Vector(5, 6, 7, 8))
    val c = a + b
    
    c should be (Matrix[Int](2, 2, Vector(6, 8, 10, 12)))
  }
}