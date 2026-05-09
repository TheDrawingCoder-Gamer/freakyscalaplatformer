package gay.menkissing

import org.lwjgl.system.MemoryStack.*
import org.lwjgl.system.MemoryUtil.*
import scala.util.Using
import engine.graphics.PrimitiveType
import engine.graphics.Mesh2D

object CustomMeshes {
  val triangleMesh =
    Using(stackPush()) { stack =>
      val verts = stack.floats(
        1f,0f,1f,1f,
        0f,0f,0f,1f,
        0.5f,1f,1f,0f
      )
      val indices = stack.ints(
        0,1,2
      )
      Mesh2D.textured(memByteBuffer(verts), memByteBuffer(indices), PrimitiveType.Triangles)
    }.get
}
