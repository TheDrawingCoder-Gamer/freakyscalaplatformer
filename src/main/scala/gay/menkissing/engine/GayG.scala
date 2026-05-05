package gay.menkissing.engine

import org.lwjgl.openal.*
import AL10.*
import ALC10.*
import java.nio.ByteBuffer
import gay.menkissing.engine.audio.SoundManager

object GayG {
  val input = Input(0)
  lazy val alManager = SoundManager()
}
