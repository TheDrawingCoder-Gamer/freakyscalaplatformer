package gay.menkissing.engine.graphics

import org.joml.Matrix4f
import org.lwjgl.opengl.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL13.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*
import org.lwjgl.stb.*
import org.lwjgl.stb.STBImage.*
import org.lwjgl.system.*
import org.lwjgl.system.MemoryStack.*
import org.lwjgl.system.MemoryUtil.*
import java.io.*
import java.nio.*
import scala.collection.mutable as mut
import scala.collection.mutable.Stack
import scala.util.Using
import gay.menkissing.engine.graphics.PrimitiveType

// should free the buffer after this
class GPUBuffer(buf: ByteBuffer, kind: Int, expectedUse: Int) extends Closeable {
  var closed = false
  val buffer = glGenBuffers()
 
  glBindBuffer(kind, buffer)
  glBufferData(kind, buf, expectedUse)

  // A non idempotent version of close
  def dispose(): Unit = {
    glDeleteBuffers(buffer)
  }

  def close(): Unit = {
    if (!closed) {
      closed = true
      dispose()
    }
  }
}