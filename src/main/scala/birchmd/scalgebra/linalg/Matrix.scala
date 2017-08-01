package birchmd.scalgebra.linalg

import birchmd.scalgebra.Field

import scala.collection.mutable
import scala.math.min

class Matrix[T](override val nrows: Int,
                          override val ncols: Int,
                          override val data: IndexedSeq[T])(implicit field: Field[T])
  extends PreMatrix[T](nrows, ncols, data)(field){

  def colBind(other: Matrix[T]): Matrix[T] = {
    val totalCols = ncols + other.ncols
    val newCols = (cols ++ other.cols).map(_.toIndexedSeq).toIndexedSeq
    val newData = Iterator.range(0, nrows).flatMap(i => {
      Iterator.range(0, totalCols).map(j => newCols(j)(i))
    })
    Matrix(nrows, totalCols, newData.toIndexedSeq)
  }

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

  def reducedRowEchelonForm: Matrix[T] = {
    //temp copy of the data to modify during the row reduction
    val tempData: mutable.IndexedSeq[mutable.IndexedSeq[T]] =
      mutable.IndexedSeq(rows.map(r => mutable.IndexedSeq(r.toSeq: _*)).toSeq:_*)
    def tempCol(j: Int): Iterator[T] = tempData.iterator.map(_.apply(j))
    def tempRowSwap(i: Int, j: Int): Unit = {
      if (i != j) {
        val rowi = tempData(i)
        tempData(i) = tempData(j)
        tempData(j) = rowi
      }
    }
    def tempRowMult(i: Int, k: T): Unit = {
      val rowi = tempData(i)
      Iterator.range(0, ncols).foreach(j => {
        rowi(j) = field.times(rowi(j), k)
      })
    }
    def tempRowAddMult(i: Int, k: T, i2: Int): Unit = {
      val rowi = tempData(i)
      val rowi2 = tempData(i2)
      Iterator.range(0, ncols).foreach(j => {
        rowi2(j) = field.plus(rowi2(j), field.times(rowi(j), k))
      })
    }

    var leading1Count: Int = 0
    Iterator.range(0, ncols).foreach(j => {
      val rowInd = tempCol(j).drop(leading1Count)
        .zipWithIndex
        .find(_._1 != field.zero) //find row with non-zero element in column j
        .map(_._2 + leading1Count) //keep index of that row
      if(rowInd.isDefined){
        tempRowSwap(rowInd.get, leading1Count) //swap found row up to row position j
        tempRowMult(leading1Count, field.multInv(tempData(leading1Count)(j))) //divide row by first non-zero element
        Iterator.range(0, nrows).foreach(i => { //kill elements in col j above and below row
          if (i != leading1Count) tempRowAddMult(leading1Count, field.addInv(tempData(i)(j)), i)
        })
        leading1Count += 1
      } //don't need to do anything if no row is found; column of zeros is fine
    })

    Matrix(nrows, ncols, tempData.iterator.flatMap(_.toIterator).toIndexedSeq)
  }

  //TODO: add row reduction functions; inverse and determinant functions (possibly in a SquareMatrix subclass?)
}

object Matrix {
  def apply[T](nrows: Int, ncols: Int, data: IndexedSeq[T])(implicit field: Field[T]): Matrix[T] =
    new Matrix(nrows, ncols, data)
}
