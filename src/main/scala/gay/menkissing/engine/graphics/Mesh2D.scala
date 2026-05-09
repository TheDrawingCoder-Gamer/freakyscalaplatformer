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
import gay.menkissing.engine.graphics.*
import gay.menkissing.engine.graphics.Color

abstract class Mesh2D(vertBuf: ByteBuffer, indexBuf: ByteBuffer, val primitiveType: Int) extends Closeable {
  var closed = false
  val drawSize: Int = indexBuf.asIntBuffer().remaining()
  val VAO = glGenVertexArrays()

  glBindVertexArray(VAO)
  val vertexBuffer = GPUBuffer(vertBuf, GL_ARRAY_BUFFER, GL_STATIC_DRAW)

  initPtrs()

  val indexBuffer = GPUBuffer(indexBuf, GL_ELEMENT_ARRAY_BUFFER, GL_STATIC_DRAW)

  glBindVertexArray(0)

  def dispose(): Unit = {
    vertexBuffer.dispose()
    indexBuffer.dispose()
    glDeleteVertexArrays(VAO)
  }

  

  def close(): Unit = {
    if (!closed) {
      closed = true
      dispose()
    }
  }

  def draw(): Unit = {
    glBindVertexArray(VAO)
    glDrawElements(primitiveType, drawSize, GL_UNSIGNED_INT, 0)
  }

  def initPtrs(): Unit
}

object Mesh2D {
  def textured(vertBuf: ByteBuffer, indexBuf: ByteBuffer, primitiveType: Int): Mesh2D =
    new Mesh2D(vertBuf, indexBuf, primitiveType) {
      def initPtrs(): Unit =
        glEnableVertexAttribArray(0)
        glVertexAttribPointer(0, 2, GL_FLOAT, false, 4 * 4, 0)
        glEnableVertexAttribArray(1)
        glVertexAttribPointer(1, 2, GL_FLOAT, false, 4 * 4, 2 * 4)
    }
  def simple(vertBuf: ByteBuffer, indexBuf: ByteBuffer, primitiveType: Int): Mesh2D =
    new Mesh2D(vertBuf, indexBuf, primitiveType) {
      def initPtrs(): Unit =
        glEnableVertexAttribArray(0)
        glVertexAttribPointer(0, 2, GL_FLOAT, false, 2 * 4, 0)
    }
}