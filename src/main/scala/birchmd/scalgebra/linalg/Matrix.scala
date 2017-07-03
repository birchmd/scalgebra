package birchmd.scalgebra.linalg

import birchmd.scalgebra.Ring

//data is a row-first 1d storage of the 2d array
//i.e. A[i, j] = data(ncols * i + j)
case class Matrix[T](nrows: Int, ncols: Int, data: IndexedSeq[T])(implicit ring: Ring[T]) {

  def apply(i: Int, j: Int): T = data(ncols * i + j)

  def row(i: Int): Iterator[T] = {
    data.iterator.slice(ncols * i, ncols * (i + 1))
  }
  def col(j: Int): Iterator[T] = {
    Iterator.range(0, nrows).map(i => data(ncols * i + j))
  }

  def rows: Iterator[Iterator[T]] = Iterator.range(0, nrows).map(i => row(i))
  def cols: Iterator[Iterator[T]] = Iterator.range(0, ncols).map(j => col(j))

  def + (other: Matrix[T]): Matrix[T] = {
    Matrix(nrows, ncols, data.zip(other.data).map{ case (a, b) => ring.plus(a, b) })
  }
  def * (k: T): Matrix[T] = {
    Matrix(nrows, ncols, data.map(a => ring.times(a, k)))
  }
  def * (other: Matrix[T]): Matrix[T] = {
    val newData: IndexedSeq[T] = (0 until nrows).flatMap(i => {
      other.cols.map(colj => {
        row(i).zip(colj)
          .map{ case (a, b) => ring.times(a, b) }
          .foldLeft(ring.zero)(ring.plus)
      })
    })
    Matrix(nrows, other.ncols, newData)
  }

  def transpose: Matrix[T] = {
    Matrix(ncols, nrows, cols.reduce(_ ++ _).toIndexedSeq)
  }

  override def toString: String = {
    rows.map(rowi => rowi.mkString(" ")).mkString("\n")
  }
}
