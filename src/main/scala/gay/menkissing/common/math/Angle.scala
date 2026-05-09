package gay.menkissing.common.math

import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordering.Implicits.infixOrderingOps

final case class Angle private (angle: Double) {
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

object Angle {
  def apply(angle: Double) = 
    new Angle(normalizeAngle(angle))
}