package gay.menkissing

import org.lwjgl.system.MemoryStack.*
import org.lwjgl.system.MemoryUtil.*
import scala.util.Using
import engine.graphics.PrimitiveType
import engine.graphics.DirectMesh2D

object CustomMeshes {
  val triangleMesh =
    Using.resource(stackPush()) { stack =>
      val verts = stack.floats(
        1f,0f,1f,1f,
        0f,0f,0f,1f,
        0.5f,1f,0.5f,0f
      )
      DirectMesh2D.textured(verts, PrimitiveType.Triangles)
    }
}
