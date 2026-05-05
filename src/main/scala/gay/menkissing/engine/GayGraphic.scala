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



sealed trait GayGraphic {
    def width: Int
    def height: Int

    def render(matrices: Matrix4f): Unit
}

final class GayRect(var width: Int, var height: Int, var color: draw.Color) extends GayGraphic {
    override def render(matrices: Matrix4f): Unit =
        draw.setSolidColor(color, matrices)
        draw.filledRect()
}

final case class GayTexture(tex: draw.Texture) extends GayGraphic {
    def width = tex.width
    def height = tex.height
    def render(matrices: Matrix4f): Unit = {
        tex.draw(matrices, 0, 0, tex.width, tex.height)
    }
}

class GayAtlas(val atlas: draw.TextureAtlas, var current: String) extends GayGraphic {
    def width = atlas(current).w
    def height = atlas(current).h
    def render(matrices: Matrix4f): Unit = {
        atlas.draw(matrices, current)
    }
}


