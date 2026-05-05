package gay.menkissing.engine.audio

import org.lwjgl.openal.AL.*
import org.lwjgl.openal.AL10.*


enum SoundFormat(val alFormat: Int) {
  case Mono8 extends SoundFormat(AL_FORMAT_MONO8)
  case Mono16 extends SoundFormat(AL_FORMAT_MONO16)
  case Stereo8 extends SoundFormat(AL_FORMAT_STEREO8)
  case Stereo16 extends SoundFormat(AL_FORMAT_STEREO16)
}
