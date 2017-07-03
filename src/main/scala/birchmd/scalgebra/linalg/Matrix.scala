package birchmd.scalgebra.linalg

import birchmd.scalgebra.Field

class Matrix[T](nrows: Int, ncols: Int, data: IndexedSeq[T])(implicit field: Field[T])
  extends PreMatrix[T](nrows, ncols, data)(field){

  //TODO: add row reduction functions; inverse and determinant functions (possibly in a SquareMatrix subclass?)
}
