package gay.menkissing.engine.audio

import org.joml.Vector3f

import org.lwjgl.openal.AL10.*

object SoundListener {
  alListener3f(AL_POSITION, 0, 0, 0)
  alListener3f(AL_VELOCITY, 0, 0, 0)

  def setSpeed(speed: Vector3f): Unit =
    alListener3f(AL_VELOCITY, speed.x, speed.y, speed.z)
  
  def setPosition(position: Vector3f): Unit =
    alListener3f(AL_POSITION, position.x, position.y, position.z)
}
