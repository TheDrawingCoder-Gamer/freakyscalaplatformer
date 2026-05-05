package gay.menkissing.engine.audio

import org.lwjgl.openal.*
import ALC10.*
import java.nio.IntBuffer

class SoundManager {
  val device = alcOpenDevice(null.asInstanceOf[String])
  if (device == 0) {
    throw new IllegalStateException("Failed to open audio device")
  }
  val deviceCaps = ALC.createCapabilities(device)
  val context = alcCreateContext(device, null.asInstanceOf[IntBuffer])
  if (context == 0) {
    throw new IllegalStateException("Failed to create or set OpenAL context")
  }
  alcMakeContextCurrent(context)
  val caps = AL.createCapabilities(deviceCaps)
}
