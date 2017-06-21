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
}
