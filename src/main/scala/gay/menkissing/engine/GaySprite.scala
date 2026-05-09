package gay.menkissing.engine

import gay.menkissing.{GameState, Player}
import gay.menkissing.common.math as gaymath
import gay.menkissing.engine.GayView
import org.joml.Matrix4f
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL13.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*
import org.lwjgl.system.*
import org.lwjgl.system.MemoryStack.*
import org.lwjgl.system.MemoryUtil.*
import scala.collection.mutable
import scala.util.Using
import gay.menkissing.draw
import gay.menkissing.engine.group.{GayContainer, GayTypedContainer}

class GaySprite(private var _graphic: GayGraphic) extends GayObject {
  def graphic: GayGraphic = _graphic


  var scaleX: Double = 1.0
  var scaleY: Double = 1.0
  override def graphicalWidth: Int = (_graphic.width * scaleX).toInt
  override def graphicalWidth_=(v: Int): Unit =
    val ratio = v / _graphic.width.toDouble 
    scaleX = ratio
  override def graphicalHeight: Int = (_graphic.height * scaleY).toInt
  override def graphicalHeight_=(v: Int): Unit =
    val ratio = v / _graphic.height.toDouble
    scaleY = ratio

  def load(graphic: GayGraphic): Unit =
    _graphic = graphic


  override def render(): Unit =  {
    if (!this.visible || !this.exists)
      return
    val width = graphicalWidth
    val cam = GayG.currentCamera
    val stack = cam.makeStack()
    cam.hotswapTo()

    val screenPos = getScreenPosition(cam)
    stack.translate(screenPos.x.toFloat, screenPos.y.toFloat, zIndex.toFloat)

    if (!facingRight) {
      stack.translate(width, 0, 0)
      stack.scale(-1, 1, 1)
    }

    stack.scale(width, graphicalHeight, 1)

    graphic.render(stack)
    
  }

}