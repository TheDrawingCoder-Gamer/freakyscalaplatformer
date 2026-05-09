package gay.menkissing.common.math

final case class RectD(x: Double, y: Double, w: Double, h: Double) {
  def offset(ox: Double, oy: Double): RectD =
    RectD(x + ox, y + oy, w, h)

  def right: Double = x + w
  def bottom: Double = y + h
  def overlaps(that: RectD): Boolean = {
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

  def contains(px: Double, py: Double): Boolean =
    px >= this.x &&
    px < this.x + this.w &&
    py >= this.y &&
    py < this.y + this.h
  def contains(p: PointF): Boolean = contains(p.x, p.y)
}
