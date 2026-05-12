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
import gay.menkissing.engine.graphics.Color
import gay.menkissing.engine.graphics.Texture
import gay.menkissing.engine.graphics.TextureAtlas



sealed trait GayGraphic {
    def width: Int
    def height: Int

    def render(matrices: Matrix4f): Unit
}

final case class GayTexture(tex: Texture) extends GayGraphic {
    def width = tex.width
    def height = tex.height
    def render(matrices: Matrix4f): Unit = {
        tex.draw(matrices, 0, 0, tex.width, tex.height)
    }
}

final class GayAtlas(val atlas: TextureAtlas, var current: String) extends GayGraphic {
    def width = atlas(current).w
    def height = atlas(current).h
    def render(matrices: Matrix4f): Unit = {
        atlas.draw(matrices, current)
    }
}


