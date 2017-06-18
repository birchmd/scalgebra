package birchmd.scalgebra

import org.scalacheck.Gen
class AdditiveIntegersMonoidTest
  extends MonoidTest[Int](Monoid.AdditiveIntegers, Gen.choose(-1000, 1000))