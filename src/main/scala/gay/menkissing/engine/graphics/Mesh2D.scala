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
import scala.io.Source
import gay.menkissing.common.math.PointF
import org.joml.Vector2f
import org.slf4j.LoggerFactory

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
  val logger = LoggerFactory.getLogger(getClass)

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

  val squareMesh = Using.resource(stackPush()) { stack =>
    val verts = stack.floats(
      0f,0f, 0.0f, 0.0f,
      1f,0f, 1.0f, 0.0f,
      0f,1f, 0.0f, 1.0f,
      1f,1f, 1.0f, 1.0f,
    )
    DirectMesh2D.textured(verts, PrimitiveType.TriangleStrip)
  }
  val squareOutlineMesh = Using.resource(stackPush()) { stack =>
    val verts = stack.floats(
        1f, 1f,
        1f, 0f,
        0f, 0f,
        0f, 1f
      )
    DirectMesh2D.simple(verts, PrimitiveType.LineLoop)
  }

  val screenMesh = Using.resource(stackPush()) { stack =>
    val verts = stack.floats(
      -1f,-1f, 0.0f, 0.0f,
      1f,-1f, 1.0f, 0.0f,
      -1f,1f, 0.0f, 1.0f,
      1f,1f, 1.0f, 1.0f,
    )
    DirectMesh2D.textured(verts, PrimitiveType.TriangleStrip)
  }

  val tau: Double = math.Pi * 2

  def setCircleVerts(vertBuf: FloatBuffer, startIdx: Int, numSegments: Int = 20): Unit = {
    for (i <- 0 to numSegments) {
      val theta = i.toDouble * tau / numSegments 
      vertBuf.put(startIdx * 2 + i * 2, math.cos(theta).toFloat)
      vertBuf.put(startIdx * 2 + i * 2 + 1, math.sin(theta).toFloat)
    }
  }

  val circleOutlineMesh = Using.resource(stackPush()) { stack =>
    val verts = stack.callocFloat(21 * 2)
    val indices = stack.callocInt(21)
    setCircleVerts(verts, 0)
    DirectMesh2D.simple(verts, PrimitiveType.LineLoop)
  }

  val circleMesh = Using.resource(stackPush()) { stack =>
    val verts = stack.callocFloat(22 * 2)
    val indices = stack.callocInt(22)
    verts.put(0, 0.0f)
    verts.put(1, 0.0f)
    indices.put(0, 0)
    setCircleVerts(verts, 1)
    DirectMesh2D.simple(verts, PrimitiveType.TriangleFan)
  }

  // This importer will ignore the "y" component and only read in the the X and Z components.
  // The object file should have only one mesh. Any more may cause weird issues
  // as they will get merged together/probably end up being incorrect
  // This importer also normalizes your mesh, so that when it's used in GayMesh2D
  // "graphicalWidth" and "graphicalHeight" are correct
  def importObj(obj: InputStream): Mesh2D =
    val data = Source.fromInputStream(obj).getLines()
    val verts = mut.Buffer.empty[Vector2f]
    val texCoords = mut.Buffer.empty[Vector2f]
    val triangles = mut.Buffer.empty[(Int, Int, Int)]
    val texTriangles = mut.Buffer.empty[(Int, Int, Int)]

 

    // We need to end up duplicating data due to OBJ not mapping directly to opengl.
    data.foreach {
      case s"v $x $_ $z" =>
        verts.append(Vector2f(x.toFloat, z.toFloat))
      case s"vt $u $v" =>
        texCoords.append(Vector2f(u.toFloat, v.toFloat))
      case s"f $p1/$t1 $p2/$t2 $p3/$t3" =>
        triangles.append((p1.toInt - 1, p2.toInt - 1, p3.toInt - 1))
        texTriangles.append((t1.toInt - 1, t2.toInt - 1, t3.toInt - 1))
      case _ => ()
    }

    val minX = verts.minBy(_.x).x
    val maxX = verts.maxBy(_.x).x
    val minY = verts.minBy(_.y).y
    val maxY = verts.maxBy(_.y).y

    val editedVerts =
      verts.map { vec =>
        Vector2f(
          1.0f * (vec.x - minX) / (maxX - minX),
          1.0f * (vec.y - minY) / (maxY - minY)
        )
      }

    val vertToTexPairing = mut.Map.empty[Int, Int]
    var validSimplified = true

    triangles.zip(texTriangles).flatMap {
      case ((a, b, c), (x, y, z)) =>
        Seq((a, x), (b, y), (c, z))
    }.foreach {
      case (p, t) =>
        if (vertToTexPairing.contains(p) && vertToTexPairing(p) != t) {
          // For some reason a vertex is reused with a different tex coord
          logger.warn(s"Vertex at index $p is reused with tex coord index ${vertToTexPairing(p)} and $t; Generating unoptimized direct mesh")
          validSimplified = false
        }
        
        vertToTexPairing.update(p, t)
    }

    val glVertexArray = mut.Buffer.empty[Float]
    if (validSimplified) {
      val glIndexArray = mut.Buffer.empty[Int]
      editedVerts.zipWithIndex.foreach { (p, idx) =>
        glVertexArray.append(p.x, p.y)
        val tex = texCoords(vertToTexPairing(idx))
        glVertexArray.append(tex.x, tex.y)
      }
      triangles.foreach {
        case (a, b, c) =>
          glIndexArray.append(a, b, c)
      }
      logger.debug(s"loaded simplified mesh with ${glVertexArray.length / 4} vertices and ${glIndexArray.length} indices, at a total of ${glVertexArray.length * 4 + glIndexArray.length * 4} bytes")

      val vertBuf = memAllocFloat(glVertexArray.length)
      vertBuf.put(0, glVertexArray.toArray)
      val idxBuf = memAllocInt(glIndexArray.length)
      idxBuf.put(0, glIndexArray.toArray)
      val mesh = Mesh2D.textured(vertBuf, idxBuf, PrimitiveType.Triangles)
      memFree(vertBuf)
      memFree(idxBuf)
      mesh
    } else {
      logger.warn("Making unoptimized mesh due to reused vertex with different texcoord")
      triangles.zip(texTriangles).foreach {
        case ((a, b, c), (x, y, z)) =>
          val p_a = editedVerts(a)
          val p_b = editedVerts(b)
          val p_c = editedVerts(c)
          val t_a = texCoords(x)
          val t_b = texCoords(y)
          val t_c = texCoords(z)
          glVertexArray
          .append(p_a.x, p_a.y, t_a.x, t_a.y)
          .append(p_b.x, p_b.y, t_b.x, t_b.y)
          .append(p_c.x, p_c.y, t_c.x, t_c.y)
      }

      logger.debug(s"loaded mesh with ${glVertexArray.length} data points, or ${glVertexArray.length * 4} bytes")

      val buf = memAllocFloat(glVertexArray.length)
      buf.put(0, glVertexArray.toArray)
      val mesh = DirectMesh2D.textured(buf, PrimitiveType.Triangles)
      memFree(buf)
      mesh
    }

}