package birchmd.scalgebra

//no difference between abeliean group and
//group from a coding perspective, but
//requires additional test in testing suite
//to ensure the operation is commutative
abstract class AbelianGroup[T] extends Group[T]
