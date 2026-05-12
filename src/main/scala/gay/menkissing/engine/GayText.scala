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
import gay.menkissing.engine.group.{GayContainer, GayTypedContainer}
import gay.menkissing.engine.graphics.Color
import gay.menkissing.Fonts
import gay.menkissing.engine.text.Font

class GayText(var text: String, var textHeight: Int, var color: Color, var font: Font = Font.defaultFont) extends GayObject {
    override def render(): Unit = {
      val cam = GayG.currentCamera
      val matrices = cam.makeStack()
      cam.hotswapTo()

      matrices.translate(x.toFloat, y.toFloat, zIndex.toFloat)

      font.drawString(text, color, matrices, textHeight)
        

    }
}