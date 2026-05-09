package gay.menkissing.engine.graphics

import org.lwjgl.opengl.GL11.*

object PrimitiveType {
  inline val Triangles = GL_TRIANGLES
  inline val TriangleStrip = GL_TRIANGLE_STRIP
  inline val TriangleFan = GL_TRIANGLE_FAN
  inline val Lines = GL_LINES
  inline val LineLoop = GL_LINE_LOOP
  inline val LineStrip = GL_LINE_STRIP
}
