package birchmd.scalgebra

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class RingTest[T] (val ring: Ring[T],
                   val gen: Gen[T],
                   val theStructure: String = "A ring")
  extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  import ring.ArithmeticOps

  theStructure should "be an abelian group under addition" in {
    val groupTest = new AbelianGroupTest[T](ring.additiveAbelianGroup, gen, "An abelian group under addition")

    groupTest.execute()
  }


  it should "be a monoid under multiplication" in {
    val monoidTest = new MonoidTest[T](ring.multiplicativeMonoid, gen, "A monoid under multiplication")

    monoidTest.execute()
  }

  it should "have multiplication which is distributive with respect to addition" in {
    val genTriples = for { x <- gen; y <- gen; z <- gen } yield (x, y, z)

    forAll(genTriples) {
      case (x, y, z) =>
        (x * (y + z)) should be ((x * y) + (x * z))
        ((y + z) * x) should be ((y * x) + (z * x))
    }
  }
}
