package gay.menkissing.engine

import org.lwjgl.openal.*
import AL10.*
import ALC10.*
import java.nio.ByteBuffer
import gay.menkissing.engine.audio.SoundManager
import gay.menkissing.common.PRNG

object GayG {
  val input = Input(0)
  lazy val alManager = SoundManager()
  val random = new PRNG {
    def random(): Double = math.random()
  }
}
