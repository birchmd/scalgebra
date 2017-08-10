package birchmd.scalgebra

abstract class Group[T] extends Monoid[T] {
  /**
    * Returns the invers of `a` with respect to the
    * operation of the group. I.e. `op(a, inverse(a)) = identity`
    * @param a the element of the group to compute the inverse of
    * @return the inverse of the given element
    */
  def inverse(a: T): T
}

object Group {
  object AdditiveIntegers extends Group[Int] {
    import Monoid.{AdditiveIntegers => monoid}

    override def op(a: Int, b: Int): Int = monoid.op(a, b)
    override val identity: Int = monoid.identity

    override def inverse(a: Int): Int = -a
  }

  /**
    * I.e. group of symmetries for regular n-gon.
    * See https://en.wikipedia.org/wiki/Dihedral_group
    * The transformations are represented as (Boolean, Int), where
    * the first element is the type of the transformation and
    * the second is which one of that type it is. For an
    * element (t, i): if t = true then it is a rotation of 360i/n
    * degrees clockwise (i.e. r_i in Wikipedia); if t = false then
    * it is a flip over the ith axis (i.e. s_i in Wikipedia).
    * @param n Number of sides of the polygon
    */
  class DihedralGroup(val n: Int) extends Group[(Boolean, Int)] {
    override val identity: (Boolean, Int) = (true, 0)

    //adding indices of transformations is done by arithmetic modulo n
    //(extra addition is to ensure result is positive)
    private[this] def indexPlus(i: Int, j: Int): Int = ((i + j) % n + n) % n

    override def op(a: (Boolean, Int), b: (Boolean, Int)): (Boolean, Int) = (a, b) match {
      case ((t1, i), (t2, j)) =>
        (t1, t2) match {
          case (true, true) => //composition of rotations
            (true, indexPlus(i, j))
          case (true, false) => //rotation + flip
            (false, indexPlus(i, j))
          case (false, true) => //flip + rotation
            (false, indexPlus(i, -j))
          case (false, false) => //flip + flip
            (true, indexPlus(i, -j))
        }
    }

    override def inverse(a: (Boolean, Int)): (Boolean, Int) = a match {
      case (t, i) => if (t) (true, n - i) else (false, i)
    }
  }
}
