package gay.menkissing.common.math

import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordering.Implicits.infixOrderingOps

final case class Rect(x: Int, y: Int, w: Int, h: Int) {
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