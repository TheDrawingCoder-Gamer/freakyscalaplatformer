package gay.menkissing.draw

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

val renderWidth = 320
val renderHeight = 180

val scaleRatio = 5

// size of assets that are fullscreen
val fullRenderWidth = renderWidth * scaleRatio
val fullRenderHeight = renderHeight * scaleRatio

object VertInstances {
  val squareOutline = Using.resource(stackPush()) { stack =>
    val verts = stack.floats(
        1f, 1f,
        1f, 0f,
        0f, 0f,
        0f, 1f
      )
    DirectMesh2D.simple(verts, PrimitiveType.LineLoop)
  }
}


lazy val squareVertices = Using.resource(stackPush()) { stack =>
  val verts = stack.floats(
    0f,0f, 0.0f, 0.0f,
    1f,0f, 1.0f, 0.0f,
    0f,1f, 0.0f, 1.0f,
    1f,1f, 1.0f, 1.0f,
  )
  DirectMesh2D.textured(verts, PrimitiveType.TriangleStrip)
}
lazy val screenVertices = Using.resource(stackPush()) { stack =>
  val verts = stack.floats(
    -1f,-1f, 0.0f, 0.0f,
    1f,-1f, 1.0f, 0.0f,
    -1f,1f, 0.0f, 1.0f,
    1f,1f, 1.0f, 1.0f,
  )
  DirectMesh2D.textured(verts, PrimitiveType.TriangleStrip)
}

def setCircleVerts(vertBuf: FloatBuffer, startIdx: Int, numSegments: Int = 20): Unit = {
  for (i <- 0 to numSegments) {
    val theta = i.toDouble * tau / numSegments 
    vertBuf.put(startIdx * 2 + i * 2, math.cos(theta).toFloat)
    vertBuf.put(startIdx * 2 + i * 2 + 1, math.sin(theta).toFloat)
  }
}

lazy val circleEdgeVerticies = Using.resource(stackPush()) { stack =>
  val verts = stack.callocFloat(21 * 2)
  val indices = stack.callocInt(21)
  setCircleVerts(verts, 0)
  DirectMesh2D.simple(verts, PrimitiveType.LineLoop)
}

lazy val filledCircleVerticies = Using.resource(stackPush()) { stack =>
  val verts = stack.callocFloat(22 * 2)
  val indices = stack.callocInt(22)
  verts.put(0, 0.0f)
  verts.put(1, 0.0f)
  indices.put(0, 0)
  setCircleVerts(verts, 1)
  DirectMesh2D.simple(verts, PrimitiveType.TriangleFan)
}
class MatrixStack extends Matrix4f {
  val stack = Stack[Matrix4f]()

  def pushMatrix(): MatrixStack = {
    val mat = new Matrix4f(this)
    stack.push(mat)
    this
  }
  def popMatrix(): MatrixStack = {
    set(stack.pop())
    this
  }

  def reset() = {
    stack.popAll()
    set(identity())
  }

  def scoped[T](block: => T): T = {
    Using.resource(ScopedStack()) { _ =>
      block
    }
  }
  def translated[T](x: Float, y: Float)(block: => T): T = {
    pushMatrix()
    translate(x, y, 0)
    val res = block
    popMatrix()
    res
  }
  def transformed[T](x: Float, y: Float, w: Float, h: Float)(block: => T): T = {
    pushMatrix()
    translate(x, y, 0)
    scaleXY(w, h)
    val res = block
    popMatrix()
    res
  }
  class ScopedStack extends Closeable {
    var closed = false
    pushMatrix()


    def close(): Unit = {
      if (!closed) {
        closed = true
        popMatrix()
      }
    }
    
  }
}



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

val tau: Double = math.Pi * 2


def setSolidColor(color: Color, transform: Matrix4f): Unit = {
  Shader.solidColorProgram.use()
  Shader.solidColorProgram.transformLoc.bind(transform)
  Shader.solidColorProgram.colorLoc.bind(color)
}
