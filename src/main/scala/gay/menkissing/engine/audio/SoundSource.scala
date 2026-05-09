package gay.menkissing.engine.audio

import org.lwjgl.openal.*
import AL10.*

import org.joml.Vector3f
import java.io.Closeable

class SoundSource(loop: Boolean, relative: Boolean) extends Closeable {
  val sourceId: Int = alGenSources()
  if (loop) {
    alSourcei(sourceId, AL_LOOPING, AL_TRUE)
  }
  if (relative) {
    alSourcei(sourceId, AL_SOURCE_RELATIVE, AL_TRUE)
  }



  def setBuffer(bufferId: Int): Unit =
    stop()
    alSourcei(sourceId, AL_BUFFER, bufferId)
  
  def setPosition(position: Vector3f): Unit =
    alSource3f(sourceId, AL_POSITION, position.x, position.y, position.z)
  
  def setSpeed(speed: Vector3f): Unit =
    alSource3f(sourceId, AL_VELOCITY, speed.x, speed.y, speed.z)
  
  def setGain(gain: Float): Unit =
    alSourcef(sourceId, AL_GAIN, gain)

  def play(): Unit =
    alSourcePlay(sourceId)

  def isPlaying: Boolean =
    alGetSourcei(sourceId, AL_SOURCE_STATE) == AL_PLAYING

  def pause(): Unit =
    alSourcePause(sourceId)

  def stop(): Unit =
    alSourceStop(sourceId)

  def close(): Unit =
    stop()
    alDeleteSources(sourceId)

}
