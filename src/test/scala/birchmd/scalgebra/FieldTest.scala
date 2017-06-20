package birchmd.scalgebra

import org.scalacheck.Gen

class FieldTest[T] (val field: Field[T],
                 override val gen: Gen[T],
                 override val theStructure: String = "A field")
  extends RingTest(field, gen, theStructure){

  theStructure should "be an abelian group under multiplication" in {
    //exclude zero from abelian group test since zero does not have a
    //multiplicative inverse
    val multGroupTest = new AbelianGroupTest[T](field.multiplicativeAbelianGroup,
      gen.retryUntil(_ != field.zero), "An abelian group under multiplication")

    multGroupTest.execute()
  }
}
