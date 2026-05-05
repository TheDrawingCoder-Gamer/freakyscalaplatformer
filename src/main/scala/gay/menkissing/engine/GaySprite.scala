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

class GaySprite(var graphic: GayGraphic) extends GayObject {
    override def graphicalWidth: Int = graphic.width
    override def graphicalHeight: Int = graphic.height

    override def render(): Unit =  {
        Game.instance.gamemanager.cameras.camList.withFilter(this.viewableOnCamera).foreach { cam =>
          val stack = cam.makeStack()
          cam.hotswapTo()

          val screenPos = getScreenPosition(cam)
          stack.translate(screenPos.x.toFloat, screenPos.y.toFloat, zIndex.toFloat)

          if (!facingRight) {
              stack.translate(graphic.width.toFloat, 0, 0)
              stack.scale(-1, 1, 1)
          }

          stack.scale(graphic.width, graphic.height, 1)

          graphic.render(stack)
        }
    }

}