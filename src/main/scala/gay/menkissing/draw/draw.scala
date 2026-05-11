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

// objects r lazy, but once referenced all their fields r forced
object Shaders {
  private def makeShader(source: String, kind: Int): Int =
    val shader = glCreateShader(kind)
    glShaderSource(shader, source)
    glCompileShader(shader)
    shader
  
  private def vertexShader(source: String): Int =
    makeShader(source, GL_VERTEX_SHADER)
  
  private def fragmentShader(source: String): Int =
    makeShader(source, GL_FRAGMENT_SHADER)

  private val fontVertexShader = {
    val source = 
      """
      #version 330 core
      layout (location = 0) in vec4 vertex;
      out vec2 texCoord;

      uniform mat4 projection;

      void main()
      {
        gl_Position = projection * vec4(vertex.xy, 0.0, 1.0);
        texCoord = vertex.zw;
      }
      """
    val shader = glCreateShader(GL_VERTEX_SHADER)
    glShaderSource(shader, source)
    glCompileShader(shader)
    shader
  }
  private val textVertexShader = {
    val source =
      """#version 330 core
        |layout (location = 0) in vec4 vertex; // <vec2 pos, vec2 tex>
        |out vec2 texCoord;
        |
        |uniform mat4 projection;
        |
        |void main()
        |{
        |    gl_Position = projection * vec4(vertex.xy, 0.0, 1.0);
        |    texCoord = vertex.zw;
        |}""".stripMargin
    val shader = glCreateShader(GL_VERTEX_SHADER)
    glShaderSource(shader, source)
    glCompileShader(shader)
    shader
  }
  private val defaultVertexShader = {
    val freakyCode =
      """
      #version 330 core
      layout (location = 0) in vec2 pos;
      layout (location = 1) in vec2 texPos;

      out vec2 texCoord;

      uniform mat4 transform;
      uniform mat4 texTransform;

      void main()
      {
        gl_Position = transform * vec4(pos, 0.0f, 1.0f);
        texCoord = (texTransform * vec4(texPos, 0.0f, 1.0f)).xy;
      }
      """
    val shader = glCreateShader(GL_VERTEX_SHADER)
    glShaderSource(shader, freakyCode)
    glCompileShader(shader)
    shader
  }
  private val blendFragmentShader = {
    val freakyCode =
      """
      #version 330 core
      out vec4 FragColor;
      
      in vec2 texCoord;

      uniform sampler2D ourTexture;

      void main()
      {
        vec4 texel = texture(ourTexture, texCoord);
        FragColor = texel;
      }
      """
    val shader = glCreateShader(GL_VERTEX_SHADER)
    glShaderSource(shader, freakyCode)
    glCompileShader(shader)
    shader
  }
  private val cutoutFragmentShader = {
    val freakyCode =
      """
      #version 330 core
      out vec4 FragColor;
      
      in vec2 texCoord;

      uniform sampler2D ourTexture;

      void main()
      {
        vec4 texel = texture(ourTexture, texCoord);
        if (texel.a < 0.5) {
          discard;
        }
        FragColor = texel;
      }
      """
    val shader = glCreateShader(GL_FRAGMENT_SHADER)
    glShaderSource(shader, freakyCode)
    glCompileShader(shader)
    shader
  }
  private val solidColorFragmentShader = {
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
  private val textFragmentShader = {
    val source =
      """
        |#version 330 core
        |in vec2 texCoord;
        |out vec4 color;
        |
        |uniform sampler2D text;
        |uniform vec4 textColor;
        |
        |void main()
        |{
        |    vec4 sampled = vec4(1.0, 1.0, 1.0, texture(text, texCoord).r);
        |    color = textColor * sampled;
        |}
        |""".stripMargin
    val shader = glCreateShader(GL_FRAGMENT_SHADER)
    glShaderSource(shader, source)
    glCompileShader(shader)
    shader
  }
  private val msdfFragmentShader = {
    val source =
      """
      #version 330 core
      in vec2 texCoord;
      out vec4 color;

      uniform sampler2D text;
      uniform vec4 textColor;
      uniform vec2 aemRange;
      uniform float inverseWidth;
      uniform float thresholdEm;

      float median(float r, float g, float b) {
        return max(min(r, g), min(max(r, g), b));
      }

      void main() 
      {
        vec3 msd = texture(text, texCoord).rgb;
        float sd = median(msd.r, msd.g, msd.b);
        float distanceEm = mix(aemRange[1], aemRange[0], sd);
        float opacity = clamp((thresholdEm - distanceEm) * inverseWidth + 0.5, 0.0, 1.0);
        color = textColor * opacity;
      }
      """
    val shader = glCreateShader(GL_FRAGMENT_SHADER)
    glShaderSource(shader, source)
    glCompileShader(shader)
    shader
  }
  private val tsdfFragmentShader = {
    fragmentShader(
      """
      #version 330 core
      in vec2 texCoord;
      out vec4 color;

      uniform sampler2D text;
      uniform vec4 textColor;
      uniform vec2 aemRange;
      uniform float inverseWidth;
      uniform float thresholdEm;

      void main() {
        float sd = texture(text, texCoord).r;
        float distanceEm = mix(aemRange[1], aemRange[0], sd);
        float opacity = clamp((thresholdEm - distanceEm) * inverseWidth + 0.5, 0.0, 1.0);
        color = textColor * opacity;
      }
      """
    )
  }
  private def makeProgram(vertex: Int, fragment: Int): Int =
    val program = glCreateProgram()
    glAttachShader(program, vertex)
    glAttachShader(program, fragment)
    glLinkProgram(program)
    program
  
  val cutoutProgram = makeProgram(defaultVertexShader, cutoutFragmentShader)
  val blendProgram = makeProgram(defaultVertexShader, blendFragmentShader)
  val solidColorProgram = makeProgram(defaultVertexShader, solidColorFragmentShader)
  val textProgram = makeProgram(textVertexShader, textFragmentShader)
  val fontSoftMaskProgram = makeProgram(fontVertexShader, textFragmentShader)
  val fontMSDFProgram = makeProgram(fontVertexShader, msdfFragmentShader)
  val fontTSDFProgram = makeProgram(fontVertexShader, tsdfFragmentShader)
  
  glDeleteShader(defaultVertexShader)
  glDeleteShader(cutoutFragmentShader)
  glDeleteShader(blendFragmentShader)
  glDeleteShader(solidColorFragmentShader)
  glDeleteShader(textVertexShader)
  glDeleteShader(textFragmentShader)
  glDeleteShader(fontVertexShader)
  glDeleteShader(msdfFragmentShader)
}

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
def filledCircle(): Unit = {
  filledCircleVerticies.draw()
}

def circle(): Unit = {
  circleEdgeVerticies.draw()
}

def rect(): Unit = {
  VertInstances.squareOutline.draw()
}
def filledRect(): Unit = {
  squareVertices.draw()
}

def setSolidColor(color: Color, transform: Matrix4f): Unit = {
  glUseProgram(Shaders.solidColorProgram)
  bindTransform(Shaders.solidColorProgram, transform, new Matrix4f())
  val colorLoc = glGetUniformLocation(Shaders.solidColorProgram, "solidColor")
  color.bind(colorLoc)
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

def drawText(text: String, color: Color, transform: Matrix4f): Unit = {
  glUseProgram(Shaders.textProgram)
  val projectionLocation = glGetUniformLocation(Shaders.textProgram, "projection")
  Using.resource(stackPush()) { stack =>
    val fb = stack.mallocFloat(16)
    glUniformMatrix4fv(projectionLocation, false, transform.get(fb))
  }

  val colorLoc = glGetUniformLocation(Shaders.textProgram, "textColor")
  color.bind(colorLoc)
  glActiveTexture(GL_TEXTURE0)
  glBindVertexArray(TextRendering.textVAO)

  var x = 0
  var y = TextRendering.textSize

  text.foreach { c =>
    if (c == '\n') {
      x = 0
      y += TextRendering.textSize
    } else
      TextRendering.characters.get(c.toInt).foreach { glyph =>
        val xpos = x + glyph.bearing.x
        val ypos = y - glyph.bearing.y
        val w = glyph.size.x
        val h = glyph.size.y
        if (glyph.texID != 0) {
          glBindTexture(GL_TEXTURE_2D, glyph.texID)
          glBindBuffer(GL_ARRAY_BUFFER, TextRendering.textVBO)
          Using(stackPush()) { stack =>
            val buf = stack.floats(
              xpos    , ypos, 0f, 0f,
              xpos + w,     ypos,     1f, 0f,
              xpos, ypos + h,     0f, 1f,
              xpos + w,     ypos + h, 1f, 1f,
            )
            glBufferSubData(GL_ARRAY_BUFFER, 0, buf)
          }
          glBindBuffer(GL_ARRAY_BUFFER, 0)
          glDrawArrays(GL_TRIANGLE_STRIP, 0, 4)
        }
        x += (glyph.advance >> 6)
      }
  }

  glBindVertexArray(0)
  glBindTexture(GL_TEXTURE_2D, 0)
}
