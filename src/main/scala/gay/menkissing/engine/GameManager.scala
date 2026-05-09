package gay.menkissing.engine

import gay.menkissing.draw
import gay.menkissing.engine.{GayState, GayView}
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

/**
 * Manages the entire game. SUPPOSED to be agnostic of the current game state
 */
class GameManager() {
  val stateStack: mutable.Stack[GayState] = mutable.Stack.empty
  var currentState: GayState = null

  glEnable(GL_BLEND)
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
  
  glEnable(GL_DEPTH_TEST)
  glDepthFunc(GL_LEQUAL)

  object cameras {
    val camList = mutable.Buffer[GayView]()
    var defaultCam: GayView = null

    def add(cam: GayView): GayView = {
      camList.append(cam)
      cam
    }
    def insert(cam: GayView, position: Int): GayView = {
      if (position >= camList.length) {
        return add(cam)
      }
      val pos =
        if (position < 0)
          position + camList.length
        else
          position


      camList.insert(pos, cam)


      cam
    }
  }

  val pixelCam = GayView(draw.renderWidth, draw.renderHeight)
  // Our pixel game shouldn't be using any blended textures.
  pixelCam.useDepth = true
  cameras.defaultCam = pixelCam
  val fullCam = GayView(draw.fullRenderWidth, draw.fullRenderHeight)
  // Our UI camera on the other hand, _will_ be using translucency.
  fullCam.useDepth = false

  cameras.add(pixelCam)
  cameras.add(fullCam)


  var frame = 0



  def run(): Unit = {
    if (currentState != null) {
      GayG.input.update()
      currentState.update()
      cameras.camList.foreach(_.clear())
      currentState.render()
      cameras.camList.foreach(_.finalizeRender())
    }
    frame += 1
    frame %= 32767
  }

  def switchState(to: GayState): Unit = {
    if (currentState != null) {
      currentState.destroy()
    }
    stateStack.foreach(_.destroy())
    stateStack.clear()
    currentState = to
    to.start()
  }
  /**
    * Suspend the current state, and switch to a new state.
    *
    * @param to
    */
  def pushState(to: GayState): Unit = {
    if (currentState != null) {
      stateStack.push(currentState)
    }
    currentState = to
    to.start()
  }

  /**
    * Exit the substate, and switch back to the parent.
    */
  def popState(): Unit = {
    if (currentState != null) {
      currentState.destroy()
    }
    currentState = stateStack.pop()
  }

  /**
    * Exit the current substate and enter a new one, without touching the stack.
    *
    * @param to
    */
  def swapSubstate(to: GayState): Unit = {
    if (currentState != null) {
      currentState.destroy()
    }
    currentState = to
    currentState.start()
  }

}
