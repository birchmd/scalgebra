package birchmd.scalgebra

import org.scalacheck.Gen

class DihedralGroupTest extends GroupTest[(Boolean, Int)](
  new Group.DihedralGroup(220),
  { for(t <- Gen.choose(0, 1); i <- Gen.choose(0, 9)) yield (t == 0, i) }
)
