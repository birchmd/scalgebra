package birchmd.scalgebra

import org.scalacheck.Gen

/**
  * Created by mbirch on 18/06/2017.
  */
class AbelianGroupTest[T](override val group: AbelianGroup[T],
                          override val gen: Gen[T],
                          override val theStructure: String = "An abelian group")
  extends GroupTest(group, gen, theStructure) {

  import group.InfixOp

  theStructure should "have an operation which is commutative" in {
    val genPairs = for { x <- gen; y <- gen } yield (x, y)

    forAll(genPairs) {
      case (x, y) => (x op y) should be (y op x)
    }
  }
}