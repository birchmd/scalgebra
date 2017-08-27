package birchmd.scalgebra.linalg

import birchmd.scalgebra.Field

import scala.collection.mutable

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

  protected def toMutable: Matrix.Mutable[T] =
    new Matrix.Mutable[T](ncols,
      mutable.IndexedSeq(rows.map(r => mutable.IndexedSeq(r.toSeq: _*)).toSeq:_*))

  def reducedRowEchelonForm: Matrix[T] = {
    //temp copy of the data to modify during the row reduction
    val temp = this.toMutable

    var leading1Count: Int = 0
    Iterator.range(0, ncols).foreach(j => {
      val rowInd = temp.col(j).drop(leading1Count) //only consider rows after current leading 1s
        .zipWithIndex
        .find(_._1 != field.zero) //find row with non-zero element in column j
        .map(_._2 + leading1Count) //keep index of that row
      if(rowInd.isDefined){
        temp.swapRows(rowInd.get, leading1Count) //swap found row up to under previous leading 1s
        val leadingEl = temp.data(leading1Count)(j)
        if (leadingEl != field.one) //divide row by first element to make leading 1
          temp.multRow(leading1Count, field.multInv(leadingEl))
        Iterator.range(0, nrows).foreach(i => { //kill elements in col j above and below row
          if (i != leading1Count && temp.data(i)(j) != field.zero)
            temp.addRowMult(leading1Count, field.addInv(temp.data(i)(j)), i)
        })
        leading1Count += 1
      } //don't need to do anything if no row is found; column of zeros is fine
    })

    Matrix(nrows, ncols, temp.data.iterator.flatMap(_.toIterator).toIndexedSeq)
  }

  def nullSpace: AffineSpace[T] = {
    Matrix.solveLinearSystem(this, ColumnVector.zero[T](this.nrows)).get
  }

  def rowSpace: AffineSpace[T] = {
    val rref = this.reducedRowEchelonForm
    val basis = rref.rows
      .map(_.toIndexedSeq)
      .filter(_.exists(_ != field.zero)) //filter out all zero rows
      .map(r => ColumnVector(r))
      .toIndexedSeq
    AffineSpace(ColumnVector.zero(this.ncols), basis)
  }

  def colSpace: AffineSpace[T] = this.transpose.rowSpace

  //redefine basic operations to return Matrix instead of PreMatrix
  override def transpose: Matrix[T] = Matrix.fromPreMatrix(super.transpose)

  override def minor(i: Int, j: Int): Matrix[T] = Matrix.fromPreMatrix(super.minor(i, j))

  override def *(k: T): Matrix[T] = Matrix.fromPreMatrix(super.*(k))

  def *(other: Matrix[T]): Matrix[T] = {
    Matrix.fromPreMatrix(super.*(other))
  }

  def +(other: Matrix[T]): Matrix[T] = {
    Matrix.fromPreMatrix(super.+(other))
  }

  def -(other: Matrix[T]): Matrix[T] = {
    Matrix.fromPreMatrix(super.-(other))
  }

  def colBind(other: Matrix[T]): Matrix[T] = {
    Matrix.fromPreMatrix(super.colBind(other))
  }
}

object Matrix {
  def apply[T](nrows: Int, ncols: Int, data: IndexedSeq[T])(implicit field: Field[T]): Matrix[T] =
    new Matrix(nrows, ncols, data)

  def fromPreMatrix[T](a: PreMatrix[T])(implicit field: Field[T]): Matrix[T] = {
    Matrix(a.nrows, a.ncols, a.data)
  }

  def fromCols[T](nrows: Int,
                  ncols: Int,
                  data: Iterator[Iterator[T]])(implicit field: Field[T]): Matrix[T] = {

    fromPreMatrix[T](PreMatrix.fromCols[T](nrows, ncols, data)(field))
  }

  def solveLinearSystem[T](a: Matrix[T],
                           b: ColumnVector[T])
                          (implicit field: Field[T]): Option[AffineSpace[T]] = {
    val augmentedMatrix = AugmentedMatrix(a, b).rowReduce
    val rref = augmentedMatrix.primary
    val foundSolution = ColumnVector(augmentedMatrix.secondary.data.take(a.ncols))

    if (augmentedMatrix.isInconsistent) {
      None
    } else if (augmentedMatrix.hasUniqueSolution) {
      //no basis since solution space consists of single point (i.e. 0-dimensional)
      Some(AffineSpace(foundSolution, List.empty[ColumnVector[T]]))
    } else {

      //map of bound variable column indices to their free variable coefficients
      val boundExpressions: Map[Int, Map[Int, T]] = rref.rows
        .map(_.zipWithIndex.dropWhile(_._1 == field.zero))
        .filter(_.hasNext)
        .map(indexedRow => {
          val leadingCol = indexedRow.next()._2
          val coeffs = indexedRow.map{ //basis vector coeff has opposite sign
            case (el, ind) => ind -> field.addInv(el)
          }.toMap
          leadingCol -> coeffs
        }).toMap

      val basis: Iterator[ColumnVector[T]] = Iterator.range(0, a.ncols)
        .filter(!boundExpressions.contains(_)) //filter to free variable columns
        .map(j => {
          val colData = Iterator.range(0, a.ncols).map(j2 => {
            if (j == j2) field.one else {
              boundExpressions
                .getOrElse(j2, Map.empty[Int, T])
                .getOrElse(j, field.zero)
            }
          })
          ColumnVector(colData.toIndexedSeq)
      })

      Some(AffineSpace(foundSolution, basis.toList))
    }
  }

  protected class Mutable[T](val ncols: Int, val data: mutable.IndexedSeq[mutable.IndexedSeq[T]])(implicit field: Field[T]) {
    def col(j: Int): Iterator[T] = data.iterator.map(_.apply(j))

    def swapRows(i: Int, j: Int): Unit = {
      if (i != j) {
        val rowi = data(i)
        data(i) = data(j)
        data(j) = rowi
      }
    }

    def multRow(i: Int, k: T): Unit = {
      val rowi = data(i)
      Iterator.range(0, ncols).foreach(j => {
        rowi(j) = field.times(rowi(j), k)
      })
    }

    def addRowMult(i: Int, k: T, i2: Int): Unit = {
      val rowi = data(i)
      val rowi2 = data(i2)
      Iterator.range(0, ncols).foreach(j => {
        rowi2(j) = field.plus(rowi2(j), field.times(rowi(j), k))
      })
    }
  }
}
