import org.joml.Matrix4f
import org.lwjgl.opengl.*
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.system.*
import MemoryUtil.*
import MemoryStack.*
import GL11.*
import GL13.*
import GL15.*
import GL30.*
import GL20.*

import scala.util.Using
import gay.menkissing.common.math as gaymath


class GayView(val width: Int, val height: Int) {
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

  // How this gets rendered to the screen, relative to the full render size.
  val renderMatrix = Matrix4f()

  var viewPos = gaymath.Point(0, 0)

  /**
   * How large the camera is in world size. Defaults to texture size. 
  */
  var worldSize = gaymath.Point(width, height)
  
  def free(): Unit = {
    glDeleteFramebuffers(framebuffer)
    glDeleteRenderbuffers(renderbuffer)
    glDeleteTextures(framebufferTex)

  }

  def setupRender(): Unit = {
    glBindFramebuffer(GL_FRAMEBUFFER, framebuffer)
    glViewport(0, 0, width, height)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
  }

  def finalizeRender(): Unit = {
    import draw.Shaders
    glBindFramebuffer(GL_FRAMEBUFFER, 0)
    glUseProgram(Shaders.defaultProgram)
    glBindVertexArray(draw.screenVertices.VAO)
    glActiveTexture(GL_TEXTURE0)
    glBindTexture(GL_TEXTURE_2D, framebufferTex)
    glViewport(Game.paddingLeft, Game.paddingTop, Game.windowWidth, Game.windowHeight)


    val mat = Matrix4f()

    draw.bindTransform(Shaders.defaultProgram, renderMatrix, mat)
    

    glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0)
  }
  
  def makeStack(): draw.MatrixStack = {
    val stack = draw.MatrixStack()
    stack.ortho(0, worldSize.x, worldSize.y, 0, -1, 1)
    stack.translate(-viewPos.x, -viewPos.y, 0)
    stack
  }
}
