package gay.menkissing.common.math

import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordering.Implicits.infixOrderingOps

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



def angleDifference(left: Double, right: Double): Double = (Angle(left) - Angle(right)).angle
def angleAlignment(left: Double, right: Double): Double = Angle(left).alignment(Angle(right))
def anglesContain(target: Double, angle1: Double, angle2: Double): Boolean = Angle(target).within(Angle(angle1), Angle(angle2))

def clamp[V](value: V, min: V, max: V)(using pord: PartialOrdering[V]): V = {
  if (pord.lt(value, min))
    min
  else if (pord.gt(value, max))
    max
  else
    value
}