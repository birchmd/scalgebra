package birchmd.scalgebra

trait SemiGroup[T] {
  def op (a: T, b: T): T //must be associative
  val unit: T //must have the property (x: T) => op(x, unit) == x

  def pow(base: T, exponent: BigInt): T = {
    //compute binary expansion of exponent
    val exponentBinary = exponent
      .toString(2)
      .toIndexedSeq

    @scala.annotation.tailrec
    def work(binaryExpansion: Iterator[Char], power: T = base, acc: T = unit): T = {
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
