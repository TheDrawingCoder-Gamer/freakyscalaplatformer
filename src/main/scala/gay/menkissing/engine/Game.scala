package gay.menkissing.engine

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
import gay.menkissing.draw

inline def renderWidth: Int = draw.renderWidth
inline def renderHeight: Int = draw.renderHeight
class Game {
  GLFWErrorCallback.createPrint(System.err).set()

  if ( !glfwInit() )
    throw new IllegalStateException("Unable to initialize GLFW")
    

  glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
  glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)


  val window = glfwCreateWindow(Game.windowWidth, Game.windowHeight, "gayme", NULL, NULL)

  if (window == NULL)
    throw new RuntimeException("Failed to create GLFW Window")

  val input1 = Input(0)
  
  glfwSetKeyCallback(window, (window, key, scancode, action, mods) => {
    if ( key == GLFW_KEY_ESCAPE && action == GLFW_RELEASE)
      glfwSetWindowShouldClose(window, true)
  })
  glfwSetJoystickCallback((jid: Int, event: Int) => {
    if (event == GLFW_CONNECTED) {
      input1.gamepadId = jid
    } else if (event == GLFW_DISCONNECTED) {
      if (jid == input1.gamepadId) {
        input1.gamepadId = -1
      }
    }
  })

  glfwSetFramebufferSizeCallback(window, Game.resizeWindow)

  /*
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
  */
  glfwMakeContextCurrent(window)
  glfwSwapInterval(1)
  glfwShowWindow(window)

  GL.createCapabilities()
  val (_) = GayG.alManager

  val gamemanager = new GameManager()
  
  val timer = new SyncTimer()
  def loop(): Unit = {

    glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
    glViewport(Game.paddingLeft, Game.paddingTop, Game.windowWidth, Game.windowHeight)

    while (!glfwWindowShouldClose(window)) {
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
      gamemanager.run()

      glfwSwapBuffers(window)
      glfwPollEvents()

      timer.sync(60)
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

object Game {
  var windowWidth: Int = renderWidth * 4
  var windowHeight: Int = renderHeight * 4
  var paddingLeft: Int = 0
  var paddingTop: Int = 0

  val aspectRatio: Double = draw.renderWidth.toDouble / draw.renderHeight.toDouble

  val resizeWindow: GLFWFramebufferSizeCallback = new GLFWFramebufferSizeCallback():
    override def invoke(window: Long, width: Int, height: Int): Unit =
      val windowRatio = width.toDouble / height.toDouble
      if windowRatio < aspectRatio then
        windowWidth = width
        paddingLeft = 0
        windowHeight = math.round(windowWidth / aspectRatio).toInt
        paddingTop = (height - windowHeight) / 2
      else
        windowHeight = height
        paddingTop = 0
        windowWidth = math.round(windowHeight * aspectRatio).toInt
        paddingLeft = (width - windowWidth) / 2

  
  lazy val instance = new Game()
}