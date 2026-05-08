package gay.menkissing

import org.lwjgl.system.MemoryStack.*
import org.lwjgl.system.MemoryUtil.*
import draw.Vertices
import scala.util.Using

object CustomMeshes {
  val triangleMesh =
    Using(stackPush()) { stack =>
      val verts = stack.floats(
        1f,0f,0f,1f,1f,
        0f,0f,0f,0f,1f,
        0.5f,1f,0f,1f,0f
      )
      val indices = stack.ints(
        0,1,2
      )
      Vertices(memByteBuffer(verts), memByteBuffer(indices))
    }.get
}
