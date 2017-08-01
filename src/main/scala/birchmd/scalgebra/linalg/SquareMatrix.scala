package birchmd.scalgebra.linalg

import birchmd.scalgebra.Field

import scala.collection.mutable

class SquareMatrix[T](override val nrows: Int,
                      override val data: IndexedSeq[T])
                     (implicit field: Field[T]) extends Matrix(nrows, nrows, data) {

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
