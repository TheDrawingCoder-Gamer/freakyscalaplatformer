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

case class Point(x: Int, y: Int) extends PointT[Int](Point.apply)

case class PointF(x: Double, y: Double) extends PointT[Double](PointF.apply) {
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

def approach(x: Double, target: Double, maxDelta: Double): Double = {
    if (x < target)
      (x + maxDelta) `min` target
    else
     (x - maxDelta) `max` target
}

def lerp(min: Double, max: Double, t: Double): Double = {
  (1 - t) * min + t * max
}

def sineInOut(t: Double): Double =
  -(math.cos(math.Pi * t) - 1) / 2

def modulo(a: Double, b: Double): Double = {
  var m = a % b
  if (m < 0) {
    m = if (b < 0) m - b else m + b
  }
  m
}
final val Tau: Double = math.Pi * 2
def normalizeAngle(theta: Double): Double = {
  var angle = modulo(theta, Tau)
  if (angle > math.Pi) {
    angle -= Tau
  }
  angle
}

case class Angle private (angle: Double) {
  def -(that: Angle): Angle =
    Angle(this.angle - that.angle)

  def alignment(that: Angle): Double =
    math.cos((this - that).angle)

  def within(angle1: Angle, angle2: Angle): Boolean = {
    val rAngle = modulo(modulo(angle2.angle - angle1.angle, Tau) + Tau, Tau)
    val a1 = if (rAngle >= math.Pi) angle2 else angle1
    val a2 = if (rAngle >= math.Pi) angle1 else angle2

    if (a1.angle <= a2.angle)
      this.angle >= a1.angle && this.angle <= a2.angle
    else
      this.angle >= a1.angle || this.angle <= a2.angle

  }
}

def angleDifference(left: Double, right: Double): Double = (Angle(left) - Angle(right)).angle
def angleAlignment(left: Double, right: Double): Double = Angle(left).alignment(Angle(right))
def anglesContain(target: Double, angle1: Double, angle2: Double): Boolean = Angle(target).within(Angle(angle1), Angle(angle2))

object Angle {
  def apply(angle: Double) = 
    new Angle(normalizeAngle(angle))
}

case class Rect(x: Int, y: Int, w: Int, h: Int) {
  def offset(ox: Int, oy: Int): Rect = {
    Rect(x + ox, y + oy, w, h)
  }
  def right: Int = x + w
  def bottom: Int = y + h
  def overlaps(that: Rect): Boolean = {
    val sx1 = this.x
    val sy1 = this.y
    val sx2 = this.right
    val sy2 = this.bottom

    val ox1 = that.x
    val oy1 = that.y
    val ox2 = that.right
    val oy2 = that.bottom


    sx2 > ox1 && sy2 > oy1 && sx1 < ox2 && sy1 < oy2
  }
  def contains(px: Int, py: Int): Boolean =
    px >= this.x &&
    px < this.x + this.w &&
    py >= this.y &&
    py < this.y + this.h
  def contains(p: Point): Boolean = contains(p.x, p.y)



}

