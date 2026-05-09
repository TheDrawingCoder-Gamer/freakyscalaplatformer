package gay.menkissing.common.math

import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordering.Implicits.infixOrderingOps

final case class PointF(x: Double, y: Double) extends PointT[Double](PointF.apply) {
  def /(scalar: Double) = PointF(this.x / scalar, this.y / scalar)
  

  def lengthSquared(): Double =
    math.pow(x, 2) + math.pow(y, 2)

  def length(): Double = math.sqrt(lengthSquared())

  def distanceSquared(that: PointF): Double = math.pow(this.x - that.x, 2) + math.pow(this.y - that.y, 2)

  def distance(that: PointF): Double = math.sqrt(distanceSquared(that))

  def dot(that: PointF): Double = (this.x * that.x) + (this.y * that.y)

  def approach(speedX: Double, speedY: Double, target: PointF): PointF =
    PointF(gay.menkissing.common.math.approach(this.x, target.x, speedX), gay.menkissing.common.math.approach(this.y, target.y, speedY))

  def lerp(that: PointF, t: Double): PointF = {
    PointF(gay.menkissing.common.math.lerp(this.x, that.x, t), gay.menkissing.common.math.lerp(this.y, that.y, t))
  }
  def cross(that: PointF): Double = {
    (this.x * that.y) - (this.y * that.x)
  }
  def elementDiv(that: PointF): PointF = 
    PointF(this.x / that.x, this.y / that.y)
 
  def toRadians(): Double = math.atan2(this.y, this.x)
}

object PointF {
  def fromRadians(angle: Double): PointF = {
    val x = math.cos(angle)
    val y = -math.sin(angle)
    PointF(x, y)
  }
}