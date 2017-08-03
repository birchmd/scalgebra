package birchmd.scalgebra.linalg

import birchmd.scalgebra.Ring

//This class is only a "PreMatrix" since it's elements are
//only required to be from a ring rather than a field.

//data is a row-first 1d storage of the 2d array
//i.e. A[i, j] = data(ncols * i + j)
class PreMatrix[T](val nrows: Int, val ncols: Int, val data: IndexedSeq[T])(implicit ring: Ring[T]) {

  override def equals(obj: scala.Any): Boolean = obj match {
    case otherPreMatrix: PreMatrix[T] =>
      this.nrows == otherPreMatrix.nrows &&
        this.ncols == otherPreMatrix.ncols &&
          this.data == otherPreMatrix.data
    case _ => super.equals(obj)
  }

  def apply(i: Int, j: Int): T = data(ncols * i + j)

  def row(i: Int): Iterator[T] = {
    data.iterator.slice(ncols * i, ncols * (i + 1))
  }
  def col(j: Int): Iterator[T] = {
    Iterator.range(0, nrows).map(i => data(ncols * i + j))
  }

  def rows: Iterator[Iterator[T]] = Iterator.range(0, nrows).map(i => row(i))
  def cols: Iterator[Iterator[T]] = Iterator.range(0, ncols).map(j => col(j))

  def + (other: PreMatrix[T]): PreMatrix[T] = {
    PreMatrix(nrows, ncols, data.zip(other.data).map{ case (a, b) => ring.plus(a, b) })
  }
  def - (other: PreMatrix[T]): PreMatrix[T] = {
    PreMatrix(nrows, ncols, data.zip(other.data).map{ case (a, b) => ring.plus(a, ring.addInv(b)) })
  }
  def * (k: T): PreMatrix[T] = {
    PreMatrix(nrows, ncols, data.map(a => ring.times(a, k)))
  }
  def * (other: PreMatrix[T]): PreMatrix[T] = {
    val newData: IndexedSeq[T] = (0 until nrows).flatMap(i => {
      other.cols.map(colj => {
        row(i).zip(colj)
          .map{ case (a, b) => ring.times(a, b) }
          .foldLeft(ring.zero)(ring.plus)
      })
    })
    PreMatrix(nrows, other.ncols, newData)
  }

  def transpose: PreMatrix[T] = {
    PreMatrix(ncols, nrows, cols.reduce(_ ++ _).toIndexedSeq)
  }

  /**
    * Computes the minor of the matrix. I.e. the matrix
    * resulting from deleting row i and column j.
    * @param i row to delete
    * @param j column to delete
    * @return minor M_ij
    */
  def minor(i: Int, j: Int): PreMatrix[T] = {
    val newData: Iterator[T] = rows
      .zipWithIndex
      .filter(_._2 != i)
      .flatMap{ case (row, _) =>
        row.zipWithIndex.filter(_._2 != j).map(_._1)
      }

    PreMatrix(nrows - 1, ncols - 1, newData.toVector)
  }

  def colBind(other: PreMatrix[T]): PreMatrix[T] = {
    val totalCols = ncols + other.ncols
    PreMatrix.fromCols(nrows, totalCols, cols ++ other.cols)
  }

  override def toString: String = {
    rows.map(rowi => rowi.mkString(" ")).mkString("\n")
  }
}

object PreMatrix {
  def apply[T](nrows: Int,
               ncols: Int,
               data: IndexedSeq[T])(implicit ring: Ring[T]): PreMatrix[T] =
    new PreMatrix(nrows, ncols, data)


  def fromCols[T](nrows: Int,
                  ncols: Int,
                  data: Iterator[Iterator[T]])(implicit ring: Ring[T]): PreMatrix[T] = {
    val cols = data.map(_.toIndexedSeq).toIndexedSeq
    val rowData = Iterator.range(0, nrows).flatMap(i => {
      Iterator.range(0, ncols).map(j => cols(j)(i))
    })
    PreMatrix(nrows, ncols, rowData.toIndexedSeq)
  }
}
