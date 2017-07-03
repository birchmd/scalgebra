package birchmd.scalgebra.linalg

import birchmd.scalgebra.Ring

//This class is only a "PreMatrix" since it's elements are
//only required to be from a ring rather than a field.

//data is a row-first 1d storage of the 2d array
//i.e. A[i, j] = data(ncols * i + j)
class PreMatrix[T](nrows: Int, ncols: Int, data: IndexedSeq[T])(implicit ring: Ring[T]) {

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

  override def toString: String = {
    rows.map(rowi => rowi.mkString(" ")).mkString("\n")
  }
}

object PreMatrix {
  def apply[T](nrows: Int,
               ncols: Int,
               data: IndexedSeq[T])(implicit ring: Ring[T]): PreMatrix[T] =
    new PreMatrix(nrows, ncols, data)


}
