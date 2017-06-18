package birchmd.scalgebra.util

object RussianPeasant {
  /**
    * Use case: compute a to the exponent n (both integers) by
    * repeating the multiplication operation n times.
    * @param in the input value (a in the use case above)
    * @param n the number of times to repeat the operation
    * @param op binary operation to perform
    * @param identity the identity element of the operation
    * @tparam T the type being operated on (Int in the use case above)
    * @return the result of repeating the binary operation on the input n times.
    */
  def rep[T](in: T, n: BigInt, op: (T, T) => T, identity: T): T = {
    //compute binary expansion of exponent
    val exponentBinary = n
      .toString(2)
      .toIndexedSeq

    @scala.annotation.tailrec
    def work(binaryExpansion: Iterator[Char], power: T = in, acc: T = identity): T = {
      if (binaryExpansion.hasNext) {
        val c = binaryExpansion.next()
        if (c == '1') {
          work(binaryExpansion, op(power, power), op(power, acc))
        } else {
          work(binaryExpansion, op(power, power), acc)
        }
      } else {
        acc
      }
    }

    work(exponentBinary.reverseIterator)
  }
}
