package gay.menkissing.engine

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
  GayG.globalManager = this
  val stateStack: mutable.Stack[GayState] = mutable.Stack.empty
  var currentState: GayState = null

  glEnable(GL_BLEND)
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
  
  glEnable(GL_DEPTH_TEST)
  glDepthFunc(GL_LEQUAL)

  glEnable(GL_MULTISAMPLE)


  GayG.frame = 0


  def run(): Unit = {
    if (currentState != null) {
      GayG.input.update()
      GayG.plugins.update()
      currentState.update()
      GayG.cameras.camList.foreach(_.startFrame())
      currentState.collectForRender()
      GayG.cameras.camList.foreach(_.finalizeFrame())
    }
    GayG.frame += 1
    GayG.frame %= 32767
  }

  def switchState(to: GayState): Unit = {
    GayG.signals.preStateSwitch.dispatch()
    if (currentState != null) {
      currentState.destroy()
    }
    stateStack.foreach(_.destroy())
    stateStack.clear()
    currentState = to
    to.start()
    GayG.signals.postStateSwitch.dispatch()
  }
  /**
    * Suspend the current state, and switch to a new state.
    *
    * @param to
    */
  def pushState(to: GayState): Unit = {
    GayG.signals.preStateSwitch.dispatch()
    if (currentState != null) {
      stateStack.push(currentState)
    }
    currentState = to
    to.start()
    GayG.signals.postStateSwitch.dispatch()
  }

  /**
    * Exit the substate, and switch back to the parent.
    */
  def popState(): Unit = {
    GayG.signals.preStateSwitch.dispatch()
    if (currentState != null) {
      currentState.destroy()
    }
    currentState = stateStack.pop()
    GayG.signals.postStateSwitch.dispatch()
  }

  /**
    * Exit the current substate and enter a new one, without touching the stack.
    *
    * @param to
    */
  def swapSubstate(to: GayState): Unit = {
    GayG.signals.preStateSwitch.dispatch()
    if (currentState != null) {
      currentState.destroy()
    }
    currentState = to
    currentState.start()
    GayG.signals.postStateSwitch.dispatch()
  }

}
