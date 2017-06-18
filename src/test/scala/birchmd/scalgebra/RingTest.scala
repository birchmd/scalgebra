package birchmd.scalgebra

import org.scalacheck.Gen

class RingTest[T] (val ring: Ring[T],
                   override val gen: Gen[T],
                   override val theStructure: String = "A ring")
  extends AbelianGroupTest(ring, gen, theStructure){

  import ring.InfixOps

  theStructure should "be a monoid under multiplication" in {
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
