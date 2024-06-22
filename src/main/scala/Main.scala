import org.lwjgl.*
import org.lwjgl.glfw.*
import org.lwjgl.opengl.*
import org.lwjgl.system.*

import java.nio.*

import org.lwjgl.glfw.Callbacks.*
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.system.MemoryStack.*
import org.lwjgl.system.MemoryUtil.*

import scala.util.Using

val renderWidth = draw.renderWidth
val renderHeight = draw.renderHeight
val windowWidth = renderWidth * 4
val windowHeight = renderHeight * 4
class Game {
  GLFWErrorCallback.createPrint(System.err).set()

  if ( !glfwInit() )
    throw new IllegalStateException("Unable to initialize GLFW")

  glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
  glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)


  val window = glfwCreateWindow(windowWidth, windowHeight, "gayme", NULL, NULL)

  if (window == NULL)
    throw new RuntimeException("Failed to create GLFW Window")

  glfwSetKeyCallback(window, (window, key, scancode, action, mods) => {
    if ( key == GLFW_KEY_ESCAPE && action == GLFW_RELEASE)
      glfwSetWindowShouldClose(window, true)
  })
  Using.resource(stackPush()) { stack =>
    val pwidth = stack.mallocInt(1)
    val pheight = stack.mallocInt(1)


    glfwGetWindowSize(window, pwidth, pheight)

    val vidmode = glfwGetVideoMode(glfwGetPrimaryMonitor())

    glfwSetWindowPos(
      window,
      (vidmode.width() - pwidth.get(0)) / 2,
      (vidmode.height() - pheight.get(0)) / 2
      )

  }
  glfwMakeContextCurrent(window)
  glfwSwapInterval(1)
  glfwShowWindow(window)

  GL.createCapabilities()

  val gamestate = new GameState()
  def loop(): Unit = {

    glClearColor(0.0f, 0.0f, 0.0f, 1.0f)
    glViewport(0, 0, windowWidth, windowHeight)

    while (!glfwWindowShouldClose(window)) {
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
      gamestate.run()

      glfwSwapBuffers(window)
      glfwPollEvents()

    }
  }

  def run(): Unit = {
    loop()

    glfwFreeCallbacks(window)
    glfwDestroyWindow(window)

    glfwTerminate()
    glfwSetErrorCallback(null).free()
  }
}

lazy val game = new Game()

@main def main() =
  game.run()
