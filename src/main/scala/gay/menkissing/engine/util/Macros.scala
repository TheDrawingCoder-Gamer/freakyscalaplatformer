package gay.menkissing.engine.util

import compiletime.*
import compiletime.ops.int.*
import quoted.*
import scala.runtime.Tuples

object Macros {


  type TupleN[T, N <: Int] <: Tuple =
    N match
      case 0 => EmptyTuple
      case S[n] => T *: TupleN[T, n]
  inline def parMap[A, R, N <: Int](self: TupleN[A, N])(f: A => R): TupleN[R, N] =
    Tuples.map[[x] =>> R](self.asInstanceOf[Tuple], [t] => t => f(t.asInstanceOf[A])).asInstanceOf[TupleN[R, N]]
}
