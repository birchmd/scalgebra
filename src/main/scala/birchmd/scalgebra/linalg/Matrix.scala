package birchmd.scalgebra.linalg

import birchmd.scalgebra.Field

class Matrix[T](override val nrows: Int,
                override val ncols: Int,
                override val data: IndexedSeq[T])(implicit field: Field[T])
  extends PreMatrix[T](nrows, ncols, data)(field){

  /**
    * Row operation; multiplies row i by scalar k.
    * @param i index of row to multiply
    * @param k scalar to multiply by
    * @return matrix with row operation applied
    */
  def multRow(i: Int, k: T): Matrix[T] = {
    val newData: Iterator[T] = rows.zipWithIndex.flatMap{
      case (row, ind) =>
        if (ind == i) row.map(a => field.times(a, k)) else row
    }

    Matrix(nrows, ncols, newData.toVector)
  }

  /**
    * Row operation; swaps row i and row j
    * @param i index of first row
    * @param j index of second row
    * @return matrix with row i replaced with row j and vice versa
    */
  def swapRows(i: Int, j: Int): Matrix[T] = {
    val rowi = row(i)
    val rowj = row(j)
    val newData = rows.zipWithIndex.flatMap{
      case (row, ind) =>
        if (ind == i) rowj
        else if (ind == j) rowi
        else row
    }

    Matrix(nrows, ncols, newData.toVector)
  }

  /**
    * Row operation; add a scalar multiple of row i to row j
    * @param i index of first row
    * @param k scalar to multiply first row by
    * @param j index of second row
    * @return matrix with row j replaced with k * row(i) + row(j)
    */
  def addRowMult(i: Int, k: T, j: Int): Matrix[T] = {
    val newRowj = row(i)
      .map(a => field.times(a, k))
      .zip(row(j))
      .map{ case (a, b) => field.plus(a, b) }

    val newData = rows.zipWithIndex.flatMap{
      case (row, ind) => if(ind == j) newRowj else row
    }

    Matrix(nrows, ncols, newData.toVector)
  }

  //TODO: add row reduction functions; inverse and determinant functions (possibly in a SquareMatrix subclass?)
}

object Matrix {
  def apply[T](nrows: Int, ncols: Int, data: IndexedSeq[T])(implicit field: Field[T]): Matrix[T] =
    new Matrix(nrows, ncols, data)
}
