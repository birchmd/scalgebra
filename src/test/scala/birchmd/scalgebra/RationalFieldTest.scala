package birchmd.scalgebra

import birchmd.scalgebra.numbers.Rational
import org.scalacheck.Gen

class RationalFieldTest extends FieldTest[Rational](Field.RationalField, {
  for(a <- Gen.choose(-1000, 1000); b <- Gen.choose(1, 1000)) yield Rational(a, b)
})
