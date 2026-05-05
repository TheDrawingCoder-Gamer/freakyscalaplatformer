package gay.menkissing.engine.audio

import java.nio.ByteBuffer

import org.lwjgl.stb.STBVorbis.*
import org.lwjgl.openal.ALC.*
import org.lwjgl.openal.AL.*
import org.lwjgl.openal.AL10.*
import org.lwjgl.system.MemoryUtil.*
import org.lwjgl.system.MemoryStack.*
import java.io.InputStream
import scala.util.Using
import org.lwjgl.stb.STBVorbisInfo
import gay.menkissing.engine.Input
import java.nio.ShortBuffer


class SoundBuffer(val pcm: ByteBuffer, format: SoundFormat, frequency: Int) {
  alGetError()
  val buffer = alGenBuffers()
  {
    val err = alGetError()
    if (err != AL_NO_ERROR) {
        throw new RuntimeException("Error loading file: " + err)
    }
  }
  alBufferData(buffer, format.alFormat, pcm, frequency)
  {
    val err = alGetError()
    if (err != AL_NO_ERROR) {
      throw new RuntimeException("Error loading file: " + err)
    }
  }
  def cleanup(): Unit =
    alDeleteBuffers(buffer)
    memFree(pcm)
}

object SoundBuffer {
  def readVorbis(in: InputStream, info: STBVorbisInfo): ShortBuffer = {
    val bytes = in.readAllBytes()
    val buf = memAlloc(bytes.length)
    buf.put(0, bytes)
    val res = Using(stackPush()) { stack =>
      val error = stack.ints(0)
      val decoder = stb_vorbis_open_memory(buf, error, null)
      if (decoder == 0) {
        throw new RuntimeException("Failed to open Ogg Vorbis file. Error: " + error.get(0))
      }

      stb_vorbis_get_info(decoder, info)

      val channels = info.channels()

      val lengthSamples = stb_vorbis_stream_length_in_samples(decoder)

      val result = memAllocShort(lengthSamples * channels)

      result.limit(stb_vorbis_get_samples_short_interleaved(decoder, channels, result) * channels)

      stb_vorbis_close(decoder)

      result
    }.get
    memFree(buf)
    res
  }

  def loadOgg(in: InputStream): SoundBuffer = {
    
    Using(STBVorbisInfo.malloc()) { info =>
      val vorbis = readVorbis(in, info)
      SoundBuffer(memByteBuffer(vorbis), if (info.channels() == 1) SoundFormat.Mono16 else SoundFormat.Stereo16, info.sample_rate())
    }.get
    
  }
}