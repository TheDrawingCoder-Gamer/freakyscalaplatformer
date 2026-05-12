package gay.menkissing.engine.graphics

import org.joml.Matrix4f
import org.lwjgl.opengl.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL13.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*
import org.lwjgl.stb.*
import org.lwjgl.stb.STBImage.*
import org.lwjgl.system.*
import org.lwjgl.system.MemoryStack.*
import org.lwjgl.system.MemoryUtil.*
import java.io.*
import java.nio.*
import scala.collection.mutable as mut
import scala.collection.mutable.Stack
import scala.util.Using
import gay.menkissing.engine.graphics.*
import gay.menkissing.engine.graphics.Color
import gay.menkissing.engine.graphics.Texture
import gay.menkissing.common.math as gaymath

class TextureAtlas(val texture: Texture) extends mut.HashMap[String, gaymath.Rect]() {

  def add(name: String, x: Int, y: Int, w: Int, h: Int): Unit = {
    val seg = gaymath.Rect(x, y, w, h)
    update(name, seg)
  }

  def draw(matrices: Matrix4f, name: String): Unit = {
    texture.draw(matrices, get(name).get)
  }
}

object TextureAtlas {
  def split(tex: Texture, xs: Int, ys: Int): TextureAtlas = {
    val atlas = new TextureAtlas(tex)
    if (tex.width % xs != 0 || tex.height % ys != 0)
      throw new RuntimeException("can't split unevenly")
    val itemWidth = tex.width / xs
    val itemHeight = tex.height / ys
    for (y <- 0 until ys) {
      for (x <- 0 until xs) {
        atlas.add(((ys * y) + x).toString, x * itemWidth, y * itemHeight, itemWidth, itemHeight)
      }
    }
    atlas
  }
  def splitBySize(tex: Texture, w: Int, h: Int): TextureAtlas = {
    if (tex.width % w != 0 || tex.height % h != 0)
      throw new RuntimeException("can't split unevenly")
    val atlas = new TextureAtlas(tex)
    val xs = tex.width / w
    val ys = tex.height / h
    for {
      y <- 0 until ys
      x <- 0 until xs
    } {
      atlas.add(((ys * y) + x).toString, x * w, y * h, w, h)
    }
    atlas
  }
}