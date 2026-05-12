package gay.menkissing.battle

import gay.menkissing.engine.GayRectSprite
import gay.menkissing.engine.graphics.Color
import gay.menkissing.engine.anim.GayTween
import gay.menkissing.engine.anim.TweenAccessor

class DamageBar(fullWidth: Int, fullHeight: Int, padding: Int) extends PercentBar(fullWidth, fullHeight, padding) {
  protected var _damage: Double = 1.0

  override def percentage_=(v: Double): Unit =
    super.percentage = v
    GayTween.tween(this, new TweenAccessor[DamageBar, Double] {
      def get(o: DamageBar): Double = o.damage
      def update(o: DamageBar, f: Double): Unit = o.damage = f
    }, v, 30).start()

  def damage: Double = _damage
  def damage_=(v: Double): Unit = 
    _damage = v
    updateDamage()

  val damageRect = GayRectSprite(fullWidth - padding * 2, fullHeight - padding * 2, Color.fromHex(0xFFDD0000))
  insert(damageRect, 1)
  damageRect.x += padding
  damageRect.y += padding

  override def recalculateSizes(): Unit =
    super.recalculateSizes()
    damageRect.graphicalWidth = ((_fullWidth - _padding * 2) * _damage).toInt
    damageRect.graphicalHeight = _fullHeight - _padding * 2
    damageRect.x = this.x + _padding
    damageRect.y = this.y + _padding

  protected def updateDamage(): Unit =
    damageRect.graphicalWidth = ((_fullWidth - _padding * 2) *_damage).toInt
}
