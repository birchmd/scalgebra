package birchmd.scalgebra.linalg

import birchmd.scalgebra.Field

class ColumnVector[T](val size: Int,
                      override val data: IndexedSeq[T])
                     (implicit field: Field[T]) extends Matrix[T](size, 1, data){
  override def col(j: Int): Iterator[T] =
    if (j == 0) data.iterator else throw new java.lang.IndexOutOfBoundsException()

  override def row(i: Int): Iterator[T] = Iterator.single(data(i))

  override def cols: Iterator[Iterator[T]] = Iterator.single(data.iterator)

  def innerProduct(other: ColumnVector[T]): T =
    data.iterator.zip(other.data.iterator).foldLeft(field.zero){
      case (acc, (a, b)) => field.plus(acc, field.times(a, b))
    }

  def outerProduct(other: ColumnVector[T]): Matrix[T] = this * other.transpose
}

object ColumnVector {
  def apply[T](data: IndexedSeq[T])(implicit field: Field[T]): ColumnVector[T] = {
    new ColumnVector[T](data.length, data)
  }
}
