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
    var currentState: GayState = null

    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    
    glEnable(GL_DEPTH_TEST)
    glDepthFunc(GL_LEQUAL)

    object cameras {
      val camList = mutable.Buffer[GayView]()
      var defaults = mutable.Buffer[GayView]()

      def add(cam: GayView, defaultDrawTarget: Boolean = true): GayView = {
        camList.append(cam)
        if (defaultDrawTarget)
          defaults.append(cam)
        cam
      }
      def insert(cam: GayView, position: Int, defaultDrawTarget: Boolean = true): GayView = {
        if (position >= camList.length) {
          return add(cam, defaultDrawTarget)
        }
        val pos =
          if (position < 0)
            position + camList.length
          else
            position


        camList.insert(pos, cam)
        if (defaultDrawTarget)
          defaults.append(cam)

        cam
      }
    }

    val pixelCam = GayView(draw.renderWidth, draw.renderHeight)
    val fullCam = GayView(draw.fullRenderWidth, draw.fullRenderHeight)

    cameras.add(pixelCam)
    cameras.add(fullCam, defaultDrawTarget = false)


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
            currentState.close()
        }
        currentState = to
        to.start()
    }

}
