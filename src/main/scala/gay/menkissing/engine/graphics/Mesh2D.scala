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

trait Mesh2D extends Closeable {
  /**
    * Bind this mesh so that OpenGL can use it for its next draw call.
    */
  def bind(): Unit
  /**
    * Draw while assuming this mesh is already bound
    * Undefined behavior if it isn't bound
    */
  def unsafeDraw(): Unit
  /**
    * Bind and draw this mesh.
    */
  def draw(): Unit =
    bind()
    unsafeDraw()
}

abstract class IndexedMesh2D(vertBuf: ByteBuffer, indexBuf: ByteBuffer, val primitiveType: Int) extends Mesh2D {
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

  def bind(): Unit =
    glBindVertexArray(VAO)

  def unsafeDraw(): Unit =
    glDrawElements(primitiveType, drawSize, GL_UNSIGNED_INT, 0)


  def initPtrs(): Unit
}

abstract class DirectMesh2D(vertBuf: ByteBuffer, val drawSize: Int, val primitiveType: Int) extends Mesh2D {
  val VAO = glGenVertexArrays()

  glBindVertexArray(VAO)
  val vertexBuffer = GPUBuffer(vertBuf, GL_ARRAY_BUFFER, GL_STATIC_DRAW)
  
  initPtrs()
  glBindVertexArray(0)
  def close(): Unit =
    vertexBuffer.close()
    glDeleteVertexArrays(VAO)

  

  def bind(): Unit =
    glBindVertexArray(VAO)
  
  def unsafeDraw(): Unit =
    glDrawArrays(primitiveType, 0, drawSize)

  def initPtrs(): Unit
}

object DirectMesh2D {
  def textured(vertBuf: FloatBuffer, primitiveType: Int): Mesh2D =
    val size = vertBuf.remaining() / 4
    new DirectMesh2D(memByteBuffer(vertBuf), size, primitiveType) {
      def initPtrs(): Unit =
        glEnableVertexAttribArray(0)
        glVertexAttribPointer(0, 2, GL_FLOAT, false, 4 * 4, 0)
        glEnableVertexAttribArray(1)
        glVertexAttribPointer(1, 2, GL_FLOAT, false, 4 * 4, 2 * 4)

    }
  def simple(vertBuf: FloatBuffer, primitiveType: Int): Mesh2D =
    val size = vertBuf.remaining() / 2
    new DirectMesh2D(memByteBuffer(vertBuf), size, primitiveType) {
      def initPtrs(): Unit =
        glEnableVertexAttribArray(0)
        glVertexAttribPointer(0, 2, GL_FLOAT, false, 2 * 4, 0)
    }
}

object Mesh2D {
  def textured(vertBuf: FloatBuffer, indexBuf: IntBuffer, primitiveType: Int): Mesh2D =
    new IndexedMesh2D(memByteBuffer(vertBuf), memByteBuffer(indexBuf), primitiveType) {
      def initPtrs(): Unit =
        glEnableVertexAttribArray(0)
        glVertexAttribPointer(0, 2, GL_FLOAT, false, 4 * 4, 0)
        glEnableVertexAttribArray(1)
        glVertexAttribPointer(1, 2, GL_FLOAT, false, 4 * 4, 2 * 4)
    }
  def simple(vertBuf: FloatBuffer, indexBuf: IntBuffer, primitiveType: Int): Mesh2D =
    new IndexedMesh2D(memByteBuffer(vertBuf), memByteBuffer(indexBuf), primitiveType) {
      def initPtrs(): Unit =
        glEnableVertexAttribArray(0)
        glVertexAttribPointer(0, 2, GL_FLOAT, false, 2 * 4, 0)
    }
}