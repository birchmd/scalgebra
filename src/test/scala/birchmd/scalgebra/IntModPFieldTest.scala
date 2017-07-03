package birchmd.scalgebra

import org.scalacheck.Gen

class IntModPFieldTest extends FieldTest[Long](Field.IntModPField(191), Gen.choose(0,191))