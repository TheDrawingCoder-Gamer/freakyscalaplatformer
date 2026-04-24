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


import scala.collection.mutable

/**
 * Manages the entire game. SUPPOSED to be agnostic of the current game state
 */
class GameManager() {
    var currentState: GayState = null

    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

    object GayViews {
      val camList = mutable.Buffer[GayView]()
      val defaults = mutable.Set[GayView]()

      def add(cam: GayView, defaultDrawTarget: Boolean = true): GayView = {
        camList.append(cam)
        if (defaultDrawTarget)
          defaults.add(cam)
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
          defaults.add(cam)

        cam
      }
    }

    val pixelCam = GayView(draw.renderWidth, draw.renderHeight)
    val fullCam = GayView(draw.fullRenderWidth, draw.fullRenderHeight)

    GayViews.add(pixelCam)
    GayViews.add(fullCam, defaultDrawTarget = false)


    var frame = 0



    def run(): Unit = {
        if (currentState != null) {
            currentState.run()
            currentState.render()
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
