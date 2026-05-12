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

object GayG {
  val input = Input(0)
  lazy val alManager = SoundManager()
  val random = new PRNG {
    def random(): Double = math.random()
  }

  def manager: GameManager = Game.instance.gamemanager
  var currentCamera: GayView = null

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
  }

  object signals {
    val preStateSwitch: GaySignal0 = new GaySignal0
    val postStateSwitch: GaySignal0 = new GaySignal0
    
    
  }
}
