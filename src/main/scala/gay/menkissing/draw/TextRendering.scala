package gay.menkissing.draw

import gay.menkissing.common.math as gaymath
import org.lwjgl.opengl.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL13.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*
import org.lwjgl.system.*
import org.lwjgl.system.MemoryStack.*
import org.lwjgl.system.MemoryUtil.*
import org.lwjgl.util.freetype.*
import org.lwjgl.util.freetype.FreeType.*

import scala.collection.mutable

final case class Glyph(texID: Int, size: gaymath.Point, bearing: gaymath.Point, advance: Int)

object TextRendering {
  val textSize = 48
  
  val characters =
    val stack = stackPush()
    val map = mutable.Map[Int, Glyph]()
    try
      val ft = stack.mallocPointer(1)
      {
        val err = FT_Init_FreeType(ft)
        if (err != FT_Err_Ok) {
          throw new LinkageError("Failed to init FreeType: " + FT_Error_String(err))
        }
      }

      val ftLong = ft.get(0)

      val (faceBuf, face) = {
        val file = getClass.getResourceAsStream("/MavenPro-Regular.ttf")
        val bytes = file.readAllBytes()
        val buf = memAlloc(bytes.length)
        buf.put(bytes)
        buf.position(0)
        val fbuf = stack.mallocPointer(1)
        val err = FT_New_Memory_Face(ftLong, buf, 0, fbuf)
        if (err != FT_Err_Ok) {
          println(err)
          throw new IllegalStateException("Failed to make font: " + FT_Error_String(err))
        }
        (buf, FT_Face.create(fbuf.get(0)))
      }

      FT_Set_Pixel_Sizes(face, 0, textSize)


      glPixelStorei(GL_UNPACK_ALIGNMENT, 1)
      for (c <- 0 until 128) {
        val err = FT_Load_Char(face, c, FT_LOAD_RENDER)
        if (err != FT_Err_Ok) {
          println("Freetype failed to load glyph: " + FT_Error_String(err))
        }

        val texture = glGenTextures()
        glBindTexture(GL_TEXTURE_2D, texture)
        val width = face.glyph().bitmap().width()
        val rows = face.glyph().bitmap().rows()
        glTexImage2D(
          GL_TEXTURE_2D,
          0,
          GL_RED,
          width,
          rows,
          0,
          GL_RED,
          GL_UNSIGNED_BYTE,
          face.glyph().bitmap().buffer(width * rows)
        )

        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP)
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP)
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)

        val glyph = Glyph(texture, gaymath.Point(width, rows), gaymath.Point(face.glyph().bitmap_left(), face.glyph().bitmap_top()), face.glyph().advance().x.toInt)
        map(c) = glyph
      }
      FT_Done_Face(face)
      memFree(faceBuf)
      FT_Done_Library(ftLong)

    finally
      stack.close()
    map.toMap

  val textVAO = glGenVertexArrays()
  val textVBO = glGenBuffers()
  glBindVertexArray(textVAO)
  glBindBuffer(GL_ARRAY_BUFFER, textVBO)
  glBufferData(GL_ARRAY_BUFFER, 4 * 6 * 4, GL_DYNAMIC_DRAW)
  glEnableVertexAttribArray(0)
  glVertexAttribPointer(0, 4, GL_FLOAT, false, 4 * 4, 0)
  glBindBuffer(GL_ARRAY_BUFFER, 0)
  glBindVertexArray(0)
}
