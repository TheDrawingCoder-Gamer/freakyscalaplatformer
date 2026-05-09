package gay.menkissing.common.math

import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordering.Implicits.infixOrderingOps

trait PointT[T: math.Numeric](ctor: (T, T) => PointT[T]) {
  val x: T
  val y: T
  
  def +(that: PointT[T]): PointT[T] = ctor(this.x + that.x, this.y + that.y)
  def -(that: PointT[T]): PointT[T] = ctor(this.x - that.x, this.y - that.y)
  def *(scalar:T): PointT[T] = ctor(this.x * scalar, this.y * scalar)
  def min(that: PointT[T]): PointT[T] = ctor(this.x `min` that.x, this.y `min` that.y)
  def max(that: PointT[T]): PointT[T] = ctor(this.x `max` that.x, this.y `max` that.y)

  def toDouble: PointF = PointF(this.x.toDouble, this.y.toDouble)
  def toInt: Point = Point(this.x.toInt, this.y.toInt)

  def withX(x: T) = ctor(x, this.y)
  def withY(y: T) = ctor(this.x, y)

  def elementTimes(that: PointT[T]): PointT[T] = 
    ctor(this.x * that.x, this.y * that.y)
}