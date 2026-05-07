package gay.menkissing
package engine

class GayRectSprite(width: Int, height: Int, color: draw.Color) extends GayObject {
  val rect = new GayRect(width, height, color)

  override def graphicalWidth: Int = rect.width
  override def graphicalWidth_=(v: Int): Unit = rect.width = v

  override def graphicalHeight: Int = rect.height
  override def graphicalHeight_=(v: Int): Unit = rect.height = v

  override def render(): Unit = {
    Game.instance.gamemanager.cameras.camList.withFilter(this.viewableOnCamera).foreach { cam =>
      val stack = cam.makeStack()
      cam.hotswapTo()

      val screenPos = getScreenPosition(cam)
      stack.translate(screenPos.x.toFloat, screenPos.y.toFloat, zIndex.toFloat)

      stack.scale(graphicalWidth, graphicalHeight, 1)

      rect.render(stack)
    }
  }
}
