package birchmd.scalgebra

import org.scalacheck.Gen

class IntRingTest extends RingTest[Int](Ring.IntRing, Gen.choose(-1000, 1000))
