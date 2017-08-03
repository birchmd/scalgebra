package birchmd.scalgebra.linalg

import birchmd.scalgebra.Field

/**
  * Represents the set of elements defined by (x - offset) in Span(basis).
  * Or geometrically (in 3D), a plane which is offset from the origin
  *
  * @param offset Vector giving the offset from zero
  * @param basis The set of vectors spanning the linear part of the space
  * @tparam T The type of elements in the set
  */
case class AffineSpace[T](offset: ColumnVector[T],
                          basis: Seq[ColumnVector[T]])(implicit field: Field[T]) {
  override def equals(obj: scala.Any): Boolean = obj match {
    case otherSpace: AffineSpace[T] =>
        if (this.basis.isEmpty || otherSpace.basis.isEmpty) {
          this.basis.size == otherSpace.basis.size &&
            this.offset == otherSpace.offset
        } else {
          //Need to determine if span(basis1) == span(basis2)
          // and (offset2 - offset1) in span(basis1).
          //Technically, bases should consist of linearly independent
          //vectors, so a quick check for a difference in size could
          //rule out equality right away. But since I never bother to check
          //if the `basis` argument is truly  a basis or not, I don't
          //think that I can rely on that assumption. So we're going to
          //use the long method of checking that all vectors of basis1
          //are in the span of basis2 and vice versa

          val baseDim = offset.size //dimension of the underlying space (i.e. number of cooridinates in vectors)
          if (baseDim != otherSpace.offset.size) return false

          val n1: Int = this.basis.size
          val basisMat1: Matrix[T] =
            Matrix.fromCols(baseDim, n1, this.basis.iterator.map(_.data.iterator))
          val n2: Int = otherSpace.basis.size
          val basisMat2: Matrix[T] =
            Matrix.fromCols(baseDim, n2, otherSpace.basis.iterator.map(_.data.iterator))

          //include offset2 - offset1 in this augmented matrix to at the same time
          //check that it lies in the span of basis1
          val aug12 = AugmentedMatrix(basisMat1,
            basisMat2.colBind(otherSpace.offset - this.offset)).rowReduce
          if (aug12.isInconsistent) {
            false
          } else {
            val aug21 = AugmentedMatrix(basisMat2, basisMat1).rowReduce

            if (aug21.isInconsistent) false else  true
          }
        }

    case _ => super.equals(obj)
  }
}
