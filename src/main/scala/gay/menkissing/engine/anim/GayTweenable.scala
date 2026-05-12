package gay.menkissing.engine.anim

import compiletime.*
import compiletime.ops.int.*
import gay.menkissing.engine.util.Macros
import Macros.TupleN
import runtime.Tuples


trait GayTweenable[O] {
  def tween(start: O, end: O, ratio: Double): O
}

object GayTweenable {



  def tweenDouble(start: Double, end: Double, ratio: Double): Double =
    val interval = end - start
    (ratio * interval) + start

  given GayTweenable[Float] {
    def tween(start: Float, end: Float, ratio: Double): Float =
      tweenDouble(start, end, ratio).toFloat
  }
  given GayTweenable[Double] {
    def tween(start: Double, end: Double, ratio: Double): Double =
      tweenDouble(start, end, ratio)
  }

  given tupleNMap[T, N <: Int](using tweenable: GayTweenable[T]): GayTweenable[TupleN[T, N]] with {
    def tween(start: TupleN[T, N], end: TupleN[T, N], ratio: Double): TupleN[T, N] =
      Macros.parMap[(T, T), T, N](Tuples.zip(start, end).asInstanceOf[TupleN[(T, T), N]])((x, y) => tweenable.tween(x, y, ratio))
  }

}
