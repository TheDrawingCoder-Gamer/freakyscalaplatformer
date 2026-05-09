package gay.menkissing.engine

import gay.menkissing.common.math as gaymath
import gay.menkissing.draw
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

import scala.util.Using
import gay.menkissing.engine.graphics.Color
import scala.collection.mutable
import java.io.Closeable


trait GayView extends Closeable {
    // Doing it like this so I may have a chance at removing the above framebuffer
  protected var _renderRect = gaymath.Rect(0, 0, draw.fullRenderWidth, draw.fullRenderHeight)
  protected var _renderTransform = new Matrix4f()

  def renderRect: gaymath.Rect = _renderRect
  def renderRect_=(v: gaymath.Rect): Unit =
    _renderRect = v
    _renderTransform.identity()
    _renderTransform.translate(v.x.toFloat / draw.fullRenderWidth, v.y.toFloat / draw.fullRenderHeight, 0f)
    _renderTransform.scaleXY(v.w.toFloat / draw.fullRenderWidth, v.h.toFloat / draw.fullRenderHeight)


  var viewPos: gaymath.Point = gaymath.Point(0, 0)
  var worldSize: gaymath.Point


  var clearColor: Color = Color.fromHex(0x00000000)
  var useDepth: Boolean = false

  val toRender = mutable.Buffer.empty[GayBasic]

  def makeStack(): draw.MatrixStack = {
    val stack = draw.MatrixStack()
    stack.mul(_renderTransform)
    stack.ortho(0, worldSize.x, worldSize.y, 0, Short.MinValue.toFloat, Short.MaxValue.toFloat)
    stack
  }

  def submitForRender(obj: GayBasic): Unit =
    toRender.append(obj)

  def hotswapTo(): Unit =
    GayG.currentCamera = this

  def startFrame(): Unit =
    toRender.clear()

  def finalizeFrame(): Unit =
    hotswapTo()
    toRender.foreach(_.render())
}

class PixelPerfectGayView(val width: Int, val height: Int) extends GayView {
  val framebufferTex = glGenTextures()
  glBindTexture(GL_TEXTURE_2D, framebufferTex)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP)
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0)
  glBindTexture(GL_TEXTURE_2D, 0)

  val renderbuffer = glGenRenderbuffers()
  glBindRenderbuffer(GL_RENDERBUFFER, renderbuffer)
  glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, width, height)
  glBindRenderbuffer(GL_RENDERBUFFER, 0)

  val framebuffer = glGenFramebuffers()
  glBindFramebuffer(GL_FRAMEBUFFER, framebuffer)
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, framebufferTex, 0)
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, renderbuffer)
  glBindFramebuffer(GL_FRAMEBUFFER, 0)


  /**
   * How large the camera is in world size. Defaults to texture size. 
  */
  var worldSize = gaymath.Point(width, height)


  
  def close(): Unit = {
    glDeleteFramebuffers(framebuffer)
    glDeleteRenderbuffers(renderbuffer)
    glDeleteTextures(framebufferTex)

  }

  override def startFrame(): Unit = {
    super.startFrame()
    hotswapTo()
    glClearColor(clearColor.r, clearColor.g, clearColor.b, clearColor.a)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
  }
  
  // WE are constantly changing our frame buffer
  override def hotswapTo(): Unit = {
    super.hotswapTo()
    glBindFramebuffer(GL_FRAMEBUFFER, framebuffer)
    glViewport(0, 0, width, height)
    if (useDepth) {
      glEnable(GL_DEPTH_TEST)
    } else {
      glDisable(GL_DEPTH_TEST)
    }
    
  }

  override def finalizeFrame(): Unit = {
    import draw.Shaders
    super.finalizeFrame()
    glBindFramebuffer(GL_FRAMEBUFFER, 0)
    glUseProgram(Shaders.cutoutProgram)
    draw.screenVertices.bind()
    glActiveTexture(GL_TEXTURE0)
    glBindTexture(GL_TEXTURE_2D, framebufferTex)
    glViewport(Game.paddingLeft, Game.paddingTop, Game.windowWidth, Game.windowHeight)


    val mat = Matrix4f()


    draw.bindTransform(Shaders.cutoutProgram, mat, mat)
    

    glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0)
  }
}

class DirectGayView(val width: Int, val height: Int) extends GayView {
  var worldSize = gaymath.Point(width, height)

  def close(): Unit = ()

  // TODO: clear color?
  override def startFrame(): Unit = super.startFrame()

  override def hotswapTo(): Unit =
    super.hotswapTo()
    glBindFramebuffer(GL_FRAMEBUFFER, 0)
    glViewport(Game.paddingLeft, Game.paddingTop, Game.windowWidth, Game.windowHeight)
    if (useDepth) {
      glEnable(GL_DEPTH_TEST)
    } else {
      glDisable(GL_DEPTH_TEST)
    }
}

