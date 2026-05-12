package gay.menkissing.engine

import org.lwjgl.openal.*
import AL10.*
import ALC10.*
import java.nio.ByteBuffer
import gay.menkissing.engine.audio.SoundManager
import gay.menkissing.common.PRNG

import collection.mutable
import scala.reflect.ClassTag
import gay.menkissing.engine.util.GaySignal0
import gay.menkissing.engine.util.GayUtil

object GayG {
  val input = Input(0)
  lazy val alManager = SoundManager()
  val random = new PRNG {
    def random(): Double = math.random()
  }

  var frame: Int = 0

  var globalManager: GameManager = null

  var currentCamera: GayView = null

  def currentState: GayState = globalManager.currentState

  var initialCamera: GayView = null

  object cameras {
    val camList = mutable.Buffer.empty[GayView]
    var defaultCam: GayView = null

    def add(cam: GayView, replaceAsDefault: Boolean = false): cam.type =
      camList.append(cam)
      if (replaceAsDefault)
        defaultCam = cam
      cam
    def insert(cam: GayView, position: Int, replaceAsDefault: Boolean = false): cam.type =
      GayUtil.insertSafe(camList, cam, position)
      if (replaceAsDefault)
        defaultCam = cam
      cam
  }

  object plugins {
    var list: mutable.ArrayBuffer[GayBasic] = mutable.ArrayBuffer.empty[GayBasic]

    def addPlugin[T <: GayBasic](plugin: T): plugin.type =
      list.append(plugin)
      plugin
    
    def get[T <: GayBasic](using tag: ClassTag[T]): Option[T] =
      list.find {
        case tag(x) => true
        case _ => false
      }.map(_.asInstanceOf[T])

    def update(): Unit =
      list.foreach { plugin =>
        if (plugin.exists && plugin.active)
          plugin.update()
      }

    def collectForRender(): Unit =
      list.foreach { plugin =>
        if (plugin.exists && plugin.visible)
          plugin.collectForRender()
      }
  }

  object signals {
    val preStateSwitch: GaySignal0 = new GaySignal0
    val postStateSwitch: GaySignal0 = new GaySignal0
    
    
  }

  /**
    * Clear any existing state and substate and switch to a new state
    *
    * @param to
    */
  def switchState(to: GayState): Unit =
    globalManager.switchState(to)
  
  /**
    * Suspend the current state, and swap to a new substate.
    *
    * @param to
    */
  def pushState(to: GayState): Unit =
    globalManager.pushState(to)
  
  /**
    * Exit the substate, and switch back to the parent
    */
  def popState(): Unit =
    globalManager.popState()


  /**
    * Exit the current substate, and swap to a new one,
    * without touching the state stack.
    *
    * @param to
    */
  def swapSubstate(to: GayState): Unit =
    globalManager.swapSubstate(to)
}
