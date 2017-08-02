package birchmd.scalgebra.linalg

import birchmd.scalgebra.Field

import scala.collection.mutable

class SquareMatrix[T](override val nrows: Int,
                      override val data: IndexedSeq[T])
                     (implicit field: Field[T]) extends Matrix(nrows, nrows, data) {
  def determinant: T = {
    //copy of matrix to modify while calculating determinant
    val temp = this.toMutable
    val minusOne = field.addInv(field.one)
    var result: T = field.one //keep track of result as row operations applied

    //attempt to use row operations to make matrix upper
    //triangular; then determinant is product of diagonal elements
    Iterator.range(0, ncols).foreach(j => {
      val rowInd = temp.col(j).drop(j) //consider only bottom right corner
        .zipWithIndex
        .find(_._1 != field.zero) //find row with non-zero element in column j
        .map(_._2 + j) //keep index of that row
      if (rowInd.isDefined) {
        val ri = rowInd.get
        if (ri != j){
          temp.swapRows(rowInd.get, j) //swap row up to row position j
          result = field.times(result, minusOne) //swapping rows changes sign of det
        }
        val leadingElInv = field.multInv(temp.data(j)(j))
        Iterator.range(j+1, nrows).foreach(i => { //kill elements in col j below row j
          if (temp.data(i)(j) != field.zero)
            temp.addRowMult(j, field.times(leadingElInv, field.addInv(temp.data(i)(j))), i)
        })
      } else {
        //column of zeros found, so result is zero
        return field.zero
      }
    })

    val diagonal = Iterator.range(0, nrows).map(i => temp.data(i)(i))
    field.times(result, diagonal.reduce(field.times))
  }

  def inverse: Option[SquareMatrix[T]] = {
    val augmentedMatrx: Matrix[T] = this.colBind(SquareMatrix.identity(nrows))
    val rrefCols = augmentedMatrx.reducedRowEchelonForm.cols.toSeq
    val thisReduced = Matrix.fromCols(nrows, nrows, rrefCols.iterator.take(nrows))
    if(thisReduced.rows.exists(_.forall(_ == field.zero))) {
      None
    } else {
      val result = Matrix.fromCols(nrows, nrows, rrefCols.iterator.drop(nrows)).data
      Some(SquareMatrix(nrows, result))
    }
  }
}

object SquareMatrix {
  def apply[T](n: Int, data: IndexedSeq[T])(implicit field: Field[T]): SquareMatrix[T] = {
    new SquareMatrix[T](n, data)
  }

  def identity[T](n: Int)(implicit field: Field[T]): SquareMatrix[T] = {
    val data = mutable.IndexedSeq.fill(n * n)(field.zero)
    Iterator.range(0, n).foreach(i => {
      data((n + 1) * i) = field.one
    })

    SquareMatrix(n, data.toIndexedSeq)
  }
}
