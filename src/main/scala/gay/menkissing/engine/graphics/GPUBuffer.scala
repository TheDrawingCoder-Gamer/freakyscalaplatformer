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
import gay.menkissing.engine.util.ByteBufferOutputStream

// should free the buffer after this
class GPUBuffer extends Closeable {
  var closed = false
  val buffer = glGenBuffers()
 
  //glBindBuffer(kind, buffer)
  //glBufferData(kind, buf, expectedUse)

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

object GPUBuffer {
  def apply(buf: ByteBuffer, kind: Int, expectedUse: Int): GPUBuffer =
    val buffer = new GPUBuffer()
    glBindBuffer(kind, buffer.buffer)
    glBufferData(kind, buf, expectedUse)
    buffer
  def load(in: InputStream, kind: Int, expectedUse: Int): GPUBuffer =
    val buffer = new GPUBuffer()
    glBindBuffer(kind, buffer.buffer)
    val ptr = glMapBuffer(kind, GL_WRITE_ONLY)
    val outStream = ByteBufferOutputStream(ptr)
    in.transferTo(outStream)
    glUnmapBuffer(kind)
    buffer

  trait GPUBufferLoadableFrom[T] {
    def load(in: T, kind: Int, expectedUse: Int): GPUBuffer
  }

  given GPUBufferLoadableFrom[ByteBuffer] {
    def load(in: ByteBuffer, kind: Int, expectedUse: Int): GPUBuffer =
      GPUBuffer(in, kind, expectedUse)
  }

  given GPUBufferLoadableFrom[InputStream] {
    def load(in: InputStream, kind: Int, expectedUse: Int): GPUBuffer =
      GPUBuffer.load(in, kind, expectedUse)
  }
}