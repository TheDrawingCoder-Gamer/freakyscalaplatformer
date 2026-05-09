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
import gay.menkissing.engine.graphics.Color

class GayText(var text: String, var textHeight: Int, var color: Color) extends GayObject {
    override def render(): Unit = {
        Game.instance.gamemanager.cameras.camList.withFilter(this.viewableOnCamera).foreach { cam =>
            val matrices = cam.makeStack()
            cam.hotswapTo()

            matrices.translate(x.toFloat, y.toFloat, zIndex.toFloat)
            val ratio = GayText.ratioFor(textHeight)
            matrices.scale(ratio, ratio, 1f)

            draw.drawText(text, color, matrices)
        }

    }
}

object GayText {
    def ratioFor(height: Int): Float =
        height.toFloat / draw.TextRendering.textSize.toFloat
}