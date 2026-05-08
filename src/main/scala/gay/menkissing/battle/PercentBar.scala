package gay.menkissing
package battle

import engine.group.*
import gay.menkissing.engine.GayRectSprite

class PercentBar(fullWidth: Int, fullHeight: Int, inPadding: Int) extends GayObjectContainer {
  private var _percentage: Double = 1
  private var _padding: Int = inPadding
  private var _fullHeight: Int = fullHeight
  private var _fullWidth: Int = fullWidth



  def percentage: Double = _percentage
  def percentage_=(v: Double): Unit =
    _percentage = v
    updatePercentage()

  def padding: Int = _padding
  def padding_=(v: Int): Unit =
    _padding = v
    recalculateSizes()
  override def graphicalWidth: Int = _fullWidth
  override def graphicalWidth_=(v: Int): Unit =
    _fullWidth = v
    recalculateSizes()

  override def graphicalHeight: Int = _fullHeight
  override def graphicalHeight_=(v: Int): Unit = 
    _fullHeight = v
    recalculateSizes()

  val baseRect = GayRectSprite(fullWidth, fullHeight, draw.Color.fromHex(0xFFDDDDDD))
  add(baseRect)
  val healthRect = GayRectSprite(fullWidth - inPadding * 2, fullHeight - inPadding * 2, draw.Color.fromHex(0xFF00FF00))
  add(healthRect)
  healthRect.x += inPadding
  healthRect.y += inPadding

  def recalculateSizes(): Unit =
    baseRect.graphicalWidth = _fullWidth
    baseRect.graphicalHeight = _fullHeight
    healthRect.graphicalWidth = ((_fullWidth - _padding * 2) * _percentage).toInt
    healthRect.graphicalHeight = _fullHeight - _padding * 2
    healthRect.x = this.x + _padding
    healthRect.y = this.y + _padding

  private def updatePercentage(): Unit =
    healthRect.graphicalWidth = ((_fullWidth - _padding * 2) * _percentage).toInt
}
