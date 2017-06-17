package birchmd.scalagebra

import org.scalatest.{FlatSpec, Matchers}
//import birchmd.scalgebra.Matrix

class MatrixTest extends FlatSpec with Matchers {
  "A matrix" should "add element-wise" in {
    val a = Matrix(2, 2, Vector(1, 2, 3, 4))
    val b = Matrix(2, 2, Vector(5, 6, 7, 8))
    val c = a + b
    
    c should be (Matrix(2, 2, Vector(6, 8, 10, 12)))
  }
}