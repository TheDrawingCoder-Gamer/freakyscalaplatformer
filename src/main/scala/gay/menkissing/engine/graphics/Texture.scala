package gay.menkissing
package engine.graphics

import draw.*
import common.math as gaymath

import org.joml.Matrix4f
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL13.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*
import org.lwjgl.stb.STBImage.{stbi_image_free, stbi_load_from_memory, stbi_set_flip_vertically_on_load}
import org.lwjgl.system.MemoryStack.*
import org.lwjgl.system.MemoryUtil.*

import java.io.*
import java.nio.*
import scala.util.Using

class Texture(buf: ByteBuffer, interpMin: Int, interpMag: Int, val renderMode: Texture.RenderMode) extends Closeable {
  var closed = false
  stbi_set_flip_vertically_on_load(false)
  val texture = glGenTextures()

  buf.flip()
  val (width, height) = Using.resource(stackPush()) { stack =>
    val xp = stack.callocInt(1)
    val yp = stack.callocInt(1)
    val channelsFilep = stack.callocInt(1)

    val res = stbi_load_from_memory(buf, xp, yp, channelsFilep, 4)

    if (res != null) {
      glBindTexture(GL_TEXTURE_2D, texture)
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, xp.get(0), yp.get(0), 0, GL_RGBA, GL_UNSIGNED_BYTE, res)

      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, interpMin)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, interpMag)


      stbi_image_free(res)
    } else {
      throw new RuntimeException("Couldn't load image")
    }

    (xp.get(0), yp.get(0))
  }

  def close(): Unit = {
    if (!closed) {
      closed = true
      glDeleteTextures(texture)
    }
  }

  def texTransform(sx: Int, sy: Int, sw: Int, sh: Int): Matrix4f = {
    val transTexMtx = Matrix4f()
    transTexMtx.translate(sx.toFloat / width, sy.toFloat / height, 0)
    transTexMtx.scale(sw.toFloat / width, sh.toFloat / height, 1)
    transTexMtx
  }



  def bind(): Unit = {
    glUseProgram(renderMode.program)
    glActiveTexture(GL_TEXTURE0)
    glBindTexture(GL_TEXTURE_2D, texture)
    
  }

  // Draw, while reusing the currently bound texture.
  def drawUnsafe(matrices: Matrix4f, sx: Int, sy: Int, sw: Int, sh: Int): Unit =
    val transTexMtx = texTransform(sx, sy, sw, sh)
    bindTransform(renderMode.program, matrices, transTexMtx)
    squareVertices.draw()

  def draw(matrices: Matrix4f, sx: Int, sy: Int, sw: Int, sh: Int): Unit = {
    bind()
    
    drawUnsafe(matrices, sx, sy, sw, sh)
  }

  def draw(matrices: Matrix4f, segment: gaymath.Rect): Unit = {
    draw(matrices, segment.x, segment.y, segment.w, segment.h)
  }

  def draw(matrices: Matrix4f): Unit = draw(matrices, 0, 0, width, height)
}

object Texture {
  // How does this texture need drawn?
  // WARNING: Blend is VERY bad and will cause depth buffer issues.
  // These textures should only be used under cameras that aren't
  // Using depth testing!
  enum RenderMode {
    case Cutout
    case Blend

    def program: Int =
      this match
        case Cutout => Shaders.cutoutProgram
        case Blend => Shaders.blendProgram
      
  }
  

  
  def apply(input: InputStream, min_filter: Int = GL_NEAREST, mag_filter: Int = GL_NEAREST, renderMode: RenderMode = RenderMode.Cutout): Texture = {
    // God hates us all
    val bytes = input.readAllBytes()
    val buf = memAlloc(bytes.length)
    buf.put(bytes)
    val tex = new Texture(buf, min_filter, mag_filter, renderMode)
    memFree(buf)
    tex
  }
}