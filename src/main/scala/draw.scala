package draw

import java.nio.*
import java.io.*
import scala.collection.mutable.Stack
import scala.collection.mutable as mut

import org.joml.{Matrix4f, Matrix3f}
import org.lwjgl.system.*
import org.lwjgl.opengl.*
import org.lwjgl.stb.*
import MemoryUtil.*
import MemoryStack.*

import STBImage.*

import GL20.*
import GL11.*
import GL13.*
import GL15.*
import GL30.*

import scala.util.Using

val renderWidth = 320
val renderHeight = 180

case class GraphicsContext(stack: MatrixStack, camera: gay.menkissing.common.math.Point)

trait Renderable {
  def render(): Unit
}

// objects r lazy, but once referenced all their fields r forced
object Shaders {
  val defaultVertexShader = {
    val freakyCode =
      """
      #version 330 core
      layout (location = 0) in vec3 aPos;
      layout (location = 1) in vec2 aTexCoord;

      out vec2 texCoord;

      uniform mat4 transform;
      uniform mat4 texTransform;

      void main()
      {
        gl_Position = transform * vec4(aPos, 1.0f);
        texCoord = (texTransform * vec4(aTexCoord, 0, 1)).xy;
      }
      """
    val shader = glCreateShader(GL_VERTEX_SHADER)
    glShaderSource(shader, freakyCode)
    glCompileShader(shader)
    shader
  }
  val defaultFragmentShader = {
    val freakyCode =
      """
      #version 330 core
      out vec4 FragColor;
      
      in vec2 texCoord;

      uniform sampler2D ourTexture;

      void main()
      {
        FragColor = texture(ourTexture, texCoord);
      }
      """
    val shader = glCreateShader(GL_FRAGMENT_SHADER)
    glShaderSource(shader, freakyCode)
    glCompileShader(shader)
    shader
  }
  val solidColorFragmentShader = {
    val freakyCode =
      """
      #version 330 core
      out vec4 FragColor;

      in vec2 texCoord;

      uniform vec4 solidColor;

      void main()
      {
        FragColor = solidColor;
      }
      """
    val shader = glCreateShader(GL_FRAGMENT_SHADER)
    glShaderSource(shader, freakyCode)
    glCompileShader(shader)
    shader
  }
  val defaultProgram = {
    val program = glCreateProgram()
    glAttachShader(program, defaultVertexShader)
    glAttachShader(program, defaultFragmentShader)
    glLinkProgram(program)
    program
  }
  val solidColorProgram = {
    val program = glCreateProgram()
    glAttachShader(program, defaultVertexShader)
    glAttachShader(program, solidColorFragmentShader)
    glLinkProgram(program)
    program
  }
}

object VertInstances {
  val squareOutline = Using.resource(stackPush()) { stack =>
    val verts = stack.floats(
        1f, 1f,
        1f, 0f,
        0f, 0f,
        0f, 1f
      )
    val indices = stack.ints(
      0, 1, 2, 3, 0, 0
      )
    SimpleVerts(memByteBuffer(verts), memByteBuffer(indices))
  }
}


lazy val squareVertices = Using.resource(stackPush()) { stack =>
  val verts = stack.floats(
    1f,1f, 0.0f, 1.0f, 1.0f,
    1f,0f, 0.0f, 1.0f, 0.0f,
    0f,0f, 0.0f, 0.0f, 0.0f,
    0f,1f, 0.0f, 0.0f, 1.0f
    )
  val indices = stack.ints(
    0, 1, 3,
    1, 2, 3
    )
  Vertices(memByteBuffer(verts), memByteBuffer(indices))
}
lazy val screenVertices = Using.resource(stackPush()) { stack =>
  val verts = stack.floats(
     1f, 1f,0f, 1f, 1f,
     1f,-1f,0f, 1f, 0f,
    -1f,-1f,0f, 0f, 0f,
    -1f, 1f,0f, 0f, 1f
    )
  val indices = stack.ints(
    0, 1, 3,
    1, 2, 3
    )
  Vertices(memByteBuffer(verts), memByteBuffer(indices))
}

def setCircleVerts(vertBuf: FloatBuffer, idxBuf: IntBuffer, startIdx: Int, numSegments: Int = 20): Unit = {
  for (i <- 0 to numSegments) {
    val theta = i.toDouble * tau / numSegments 
    vertBuf.put(startIdx * 2 + i * 2, math.cos(theta).toFloat)
    vertBuf.put(startIdx * 2 + i * 2 + 1, math.sin(theta).toFloat)
    idxBuf.put(startIdx + i, startIdx + i)
  }
}

lazy val circleEdgeVerticies = Using.resource(stackPush()) { stack =>
  val verts = stack.callocFloat(21 * 2)
  val indices = stack.callocInt(21)
  setCircleVerts(verts, indices, 0)
  SimpleVerts(memByteBuffer(verts), memByteBuffer(indices))
}

lazy val filledCircleVerticies = Using.resource(stackPush()) { stack =>
  val verts = stack.callocFloat(22 * 2)
  val indices = stack.callocInt(22)
  verts.put(0, 0.0f)
  verts.put(1, 0.0f)
  indices.put(0, 0)
  setCircleVerts(verts, indices, 1)
  SimpleVerts(memByteBuffer(verts), memByteBuffer(indices))
}
// should free the buffer after this
class Buffer(buf: ByteBuffer, kind: Int, expectedUse: Int) extends Closeable {
  var closed = false
  val buffer = glGenBuffers()
 
  glBindBuffer(kind, buffer)
  glBufferData(kind, buf, expectedUse)

  // A non idempotent version of close
  def dispose(): Unit = {
    glDeleteBuffers(buffer)
  }

  def close(): Unit = {
    if (!closed) {
      closed = true
      dispose()
    }
  }
}

abstract class Vert(vertBuf: ByteBuffer, indexBuf: ByteBuffer) extends Closeable {
  var closed = false
  val VAO = glGenVertexArrays()

  glBindVertexArray(VAO)
  val vertexBuffer = Buffer(vertBuf, GL_ARRAY_BUFFER, GL_STATIC_DRAW)

  initPtrs()

  val indexBuffer = Buffer(indexBuf, GL_ELEMENT_ARRAY_BUFFER, GL_STATIC_DRAW)

  glBindVertexArray(0)

  def dispose(): Unit = {
    vertexBuffer.dispose()
    indexBuffer.dispose()
    glDeleteVertexArrays(VAO)
  }

  def close(): Unit = {
    if (!closed) {
      closed = true
      dispose()
    }
  }

  def initPtrs(): Unit
}

class Vertices(vertBuf: ByteBuffer, indexBuf: ByteBuffer) extends Vert(vertBuf, indexBuf) {
  def initPtrs(): Unit = {
    glEnableVertexAttribArray(0)
    glVertexAttribPointer(0, 3, GL_FLOAT, false, 5 * 4, 0)
    glEnableVertexAttribArray(1)
    glVertexAttribPointer(1, 2, GL_FLOAT, false, 5 * 4, 3 * 4)
  }
}

class SimpleVerts(vertBuf: ByteBuffer, indexBuf: ByteBuffer) extends Vert(vertBuf, indexBuf) {
  def initPtrs(): Unit = {
    glEnableVertexAttribArray(0)
    glVertexAttribPointer(0, 2, GL_FLOAT, false, 2 * 4, 0)
  }
}

class Texture(buf: ByteBuffer, interpMin: Int, interpMag: Int) extends Closeable {
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

  def draw(matrices: Matrix4f, sx: Int, sy: Int, sw: Int, sh: Int): Unit = {
    glUseProgram(Shaders.defaultProgram)
    glBindVertexArray(squareVertices.VAO)
    glActiveTexture(GL_TEXTURE0)
    glBindTexture(GL_TEXTURE_2D, texture)
    
    val transformLoc = glGetUniformLocation(Shaders.defaultProgram, "transform")
    val texTransformLoc = glGetUniformLocation(Shaders.defaultProgram, "texTransform")

    val transTexMtx = Matrix4f()
    transTexMtx.translate(sx.toFloat / width, sy.toFloat / height, 0)
    transTexMtx.scale(sw.toFloat / width, sh.toFloat/ height, 0)
    Using.resource(stackPush()) { stack => 
      val fb = stack.mallocFloat(16)

      glUniformMatrix4fv(transformLoc, false, matrices.get(fb))

      val fb2 = stack.mallocFloat(16)

      glUniformMatrix4fv(texTransformLoc, false, transTexMtx.get(fb2))

    }
    glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0)
  }
  def draw(matrices: Matrix4f, segment: TextureSegment): Unit = {
    draw(matrices, segment.x, segment.y, segment.w, segment.h)
  }
  def draw(matrices: Matrix4f): Unit = draw(matrices, 0, 0, width, height)
}

object Texture {
  def apply(input: InputStream, min_filter: Int = GL_NEAREST, mag_filter: Int = GL_NEAREST): Texture = {
    // God hates us all
    val bytes = input.readAllBytes()
    val buf = memAlloc(bytes.length)
    buf.put(bytes)
    val tex = new Texture(buf, min_filter, mag_filter)
    memFree(buf)
    tex
  }
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


case class TextureSegment(x: Int, y: Int, w: Int, h: Int)

class TextureAtlas(val texture: Texture) extends mut.HashMap[String, TextureSegment]() {

  def add(name: String, x: Int, y: Int, w: Int, h: Int): Unit = {
    val seg = TextureSegment(x, y, w, h)
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

final val tau: Double = math.Pi * 2
def filledCircle(): Unit = {
  glBindVertexArray(filledCircleVerticies.VAO)
  glDrawElements(GL_TRIANGLE_FAN, 22, GL_UNSIGNED_INT, 0)
}

def circle(): Unit = {
  glBindVertexArray(circleEdgeVerticies.VAO)
  glDrawElements(GL_LINE_LOOP, 21, GL_UNSIGNED_INT, 0)
}

def rect(): Unit = {
  glBindVertexArray(VertInstances.squareOutline.VAO)
  glDrawElements(GL_LINE_STRIP, 6, GL_UNSIGNED_INT, 0)
}
def filledRect(): Unit = {
  glBindVertexArray(squareVertices.VAO)
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0)
}


case class Color(r: Float, g: Float, b: Float, a: Float)

object Color {
  def fromRGBA8(r: Int, g: Int, b: Int, a: Int) = new Color(r.toFloat / 255, g.toFloat / 255, b.toFloat / 255, a.toFloat / 255)
  def fromHex(hex: Int): Color = {
    val r = (hex & 0x00FF0000) >>> 16
    val g = (hex & 0x0000FF00) >>> 8
    val b = (hex & 0x000000FF)
    val a = (hex & 0xFF000000) >>> 24
    fromRGBA8(r, g, b, a)

  }
}

def setSolidColor(color: Color, transform: Matrix4f): Unit = {
  glUseProgram(Shaders.solidColorProgram)
  bindTransform(Shaders.solidColorProgram, transform, new Matrix4f())
  val colorLoc = glGetUniformLocation(Shaders.solidColorProgram, "solidColor")
  glUniform4f(colorLoc, color.r, color.g, color.b, color.a)
}



def bindTransform(shader: Int, transform: Matrix4f, texTransform: Matrix4f): Unit = {
  val transformLoc = glGetUniformLocation(shader, "transform")
  val texTransformLoc = glGetUniformLocation(shader, "texTransform")


  Using.resource(stackPush()) { stack => 
    
    val fb = stack.mallocFloat(16)

    glUniformMatrix4fv(transformLoc, false, transform.get(fb))

    val fb2 = stack.mallocFloat(16)

    glUniformMatrix4fv(texTransformLoc, false,  texTransform.get(fb2))

  }
}
