package birchmd.scalgebra.linalg

import birchmd.scalgebra.Field

trait AugmentedMatrix[T] { self =>
  val numPrimaryCols: Int
  val mat: Matrix[T]
  implicit val field: Field[T]
  protected val isReduced: Boolean = false

  def rowReduce(implicit f: Field[T]): AugmentedMatrix[T] = new AugmentedMatrix[T] {
    override val numPrimaryCols: Int = self.numPrimaryCols
    override val mat: Matrix[T] = self.mat.reducedRowEchelonForm
    override implicit val field: Field[T] = f
    override protected val isReduced: Boolean = true
  }

  def primary: Matrix[T] =
    Matrix.fromCols(mat.nrows, numPrimaryCols, mat.cols.take(numPrimaryCols))

  def secondary: Matrix[T] =
    Matrix.fromCols(mat.nrows, mat.ncols - numPrimaryCols, mat.cols.drop(numPrimaryCols))

  /**
    * Checks if a row of zeros in the primary part of the matrix corresponds to
    * a non-zero value in the secondary part. I.e. if [A|b] is an augmented
    * matrix with A in reduced row echelon form, this function checks if
    * there a no solutions to Ax = b.
    * @return true if the linear system represented by the augmented
    *         matrix is inconsistent (has no solutions)
    */
  def isInconsistent: Boolean = {
    require(isReduced, "Status of solution must be obtained from row reduced matrix")
    mat.rows.exists(r => {
      val (leadingZeros, remainingElements) = r.span(_ == field.zero)
      leadingZeros.size >= numPrimaryCols && remainingElements.nonEmpty
    })
  }

  def hasUniqueSolution: Boolean = {
    require(isReduced, "Status of solution must be obtained from row reduced matrix")
    mat.rows.forall(r => {
      r.takeWhile(_ == field.zero).size < numPrimaryCols
    })
  }

  def hasManySolutions: Boolean = {
    require(isReduced, "Status of solution must be obtained from row reduced matrix")
    mat.rows.exists(_.forall(_ == field.zero))
  }
}

object AugmentedMatrix {
  def apply[T](left: Matrix[T], right: Matrix[T])(implicit f: Field[T]): AugmentedMatrix[T] =
    new AugmentedMatrix[T] {
      override val numPrimaryCols: Int = left.ncols
      override val mat: Matrix[T] = left.colBind(right)
      override implicit val field: Field[T] = f
    }
}
