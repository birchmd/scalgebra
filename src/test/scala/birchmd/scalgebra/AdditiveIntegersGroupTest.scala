package birchmd.scalgebra

import org.scalacheck.Gen

class AdditiveIntegersGroupTest
  extends GroupTest[Int](Group.AdditiveIntegers, Gen.choose(-1000, 1000))
