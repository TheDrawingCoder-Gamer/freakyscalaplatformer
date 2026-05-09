package gay.menkissing.common.math

import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordering.Implicits.infixOrderingOps

final case class Point(x: Int, y: Int) extends PointT[Int](Point.apply)