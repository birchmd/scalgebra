package birchmd.scalgebra

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class MonoidTest[T](val monoid: Monoid[T], val gen: Gen[T], val theStructure: String = "A monoid")
  extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  import monoid.InfixOp

  theStructure should "have an associative operation" in  {
    val genTriples = for { x <- gen; y <- gen; z <- gen } yield (x, y, z)

    forAll(genTriples){
      case (x, y, z) => ((x op y) op z) should be (x op (y op z))
    }

  }

  it should "have an identity element" in {
    val y: T = monoid.identity

    forAll(gen){
      (x: T) => (x op y) should be (x)
    }
  }
}
