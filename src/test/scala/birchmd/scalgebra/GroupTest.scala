package birchmd.scalgebra

import org.scalacheck.Gen

class GroupTest[T](val group: Group[T],
                   override val gen: Gen[T],
                   override val theStructure: String = "A group")
  extends MonoidTest[T](group, gen, theStructure) {

  import group.InfixOp
  theStructure should "have an inverse for every element" in {
    forAll(gen){
      (x: T) => (x op group.inverse(x)) should be (group.identity)
    }
  }
}
