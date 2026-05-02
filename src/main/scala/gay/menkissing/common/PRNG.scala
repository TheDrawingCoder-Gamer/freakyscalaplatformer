package gay.menkissing.common

trait PRNG {
  def random(): Double

  final def randomIn(range: Range): Int = {
    val diff = (range.end - range.start) / range.step
    val r = (random() * diff).toInt
    r * range.step + range.start
  }
  final def randomFrom[V](c: Array[V]): V = {
    val v = randomIn(c.indices)
    c(v)
  }
}
