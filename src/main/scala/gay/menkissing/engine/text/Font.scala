package gay.menkissing
package engine
package text

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


import gay.menkissing.engine.graphics.Texture
import gay.menkissing.engine.graphics.Color
import scala.util.Using
import upickle.default.*
import org.joml.Vector3f
import gay.menkissing.engine.text.Font.FontKind
import gay.menkissing.engine.graphics.Shader

class Font(val texture: Texture, val atlas: FontAtlas, val overrideProgram: Option[Shader] = None) {
  private def program: Shader =
    overrideProgram.getOrElse(atlas.atlas.kind.program)
  private def getScreenScale(textHeight: Int): Float =
    val glyph = atlas.glyphs.find(_._2.texInfo.isDefined).get._2.texInfo.get
    val inputSizePx = glyph.atlasBounds.w
    val outputSizePx = glyph.planeBounds.w * GayG.currentCamera.width
    (outputSizePx / inputSizePx).toFloat


  def drawString(text: String, color: Color, matrices: Matrix4f, textSize: Int): Unit =
    val program = atlas.atlas.kind.program
    program.use()
    program.getLocation("projection").bind(matrices)

    program.getLocation("textColor").bind(color)

    if (atlas.atlas.kind.isSDF) {
      val antialiasPerEm = atlas.atlas.size / (Game.windowWidth.toDouble / GayG.currentCamera.width)
      val scale = getScreenScale(textSize)
      // println(scale)
      program.getLocation("inverseWidth").bind((scale * antialiasPerEm).toFloat)
      val aemRange = atlas.atlas.aemRange.get
      program.getLocation("aemRange").bind(aemRange.low.toFloat, aemRange.high.toFloat)
      program.getLocation("thresholdEm").bind(0.0f)
    }

    
    glActiveTexture(GL_TEXTURE0)
    glBindVertexArray(Font.textVAO)
    texture.bind()

    var x = 0.0
    var y = textSize * atlas.metrics.lineHeight

    text.foreach { c =>
      if (c == '\n') {
        x = 0
        y += textSize * atlas.metrics.lineHeight
      } else {
        atlas.glyphs.get(c).foreach { glyph =>
          glyph.texInfo.foreach { texInfo =>
            val xpos = (x + texInfo.planeBounds.x * textSize).toFloat
            val ypos = (y + texInfo.planeBounds.y * textSize).toFloat
            val w = (texInfo.planeBounds.w * textSize).toFloat
            val h = (texInfo.planeBounds.h * textSize).toFloat
            val texX = texInfo.atlasBounds.x.toFloat / atlas.atlas.width
            val texY = texInfo.atlasBounds.y.toFloat / atlas.atlas.height
            val texWidth = texInfo.atlasBounds.w.toFloat / atlas.atlas.width
            val texHeight = texInfo.atlasBounds.h.toFloat / atlas.atlas.height

            glBindBuffer(GL_ARRAY_BUFFER, Font.textVBO)
            Using.resource(stackPush()) { stack =>
              val buf = stack.floats(
                xpos, ypos, texX, texY,
                xpos + w, ypos, texX + texWidth, texY,
                xpos, ypos + h, texX, texY + texHeight,
                xpos + w, ypos + h, texX + texWidth, texY + texHeight
              )
              glBufferSubData(GL_ARRAY_BUFFER, 0, buf)
            }
            glBindBuffer(GL_ARRAY_BUFFER, 0)
            glDrawArrays(GL_TRIANGLE_STRIP, 0, 4)
          }
          x += glyph.advance * textSize
        }
      }
    }
    glBindVertexArray(0)
    glBindTexture(GL_TEXTURE_2D, 0)

}

object Font {
  enum FontKind derives ReadWriter {
    @upickle.implicits.key("msdf")
    case MSDF
    @upickle.implicits.key("softmask")
    case SoftMask
    @upickle.implicits.key("sdf")
    case SDF
    @upickle.implicits.key("mtsdf")
    case MTSDF

    def isSDF: Boolean =
      this match
        case MSDF | SDF | MTSDF => true
        case SoftMask => false
      

    def program: Shader =
      this match
        // Ignore the true SDF in the MTSDF
        case MSDF | MTSDF => Shader.fontMSDFProgram
        case SDF => Shader.fontTSDFProgram
        case SoftMask => Shader.fontSoftMaskProgram
      
  }

  

  def load(texPath: String, atlasPath: String): Font =
    val texture = Texture(getClass.getResourceAsStream(texPath), GL_NEAREST, GL_NEAREST, Texture.RenderMode.Blend, false)
    val atlas = ujson.InputStreamParser.transform[FontAtlas](getClass.getResourceAsStream(atlasPath), upickle.default.reader[FontAtlas])
    Font(texture, atlas)

  lazy val defaultFont: Font = Font.load("/fonts/default/font.png", "/fonts/default/font.json")

  val textVAO = glGenVertexArrays()
  val textVBO = glGenBuffers()
  glBindVertexArray(textVAO)
  glBindBuffer(GL_ARRAY_BUFFER, textVBO)
  glBufferData(GL_ARRAY_BUFFER, 4 * 6 * 4, GL_STREAM_DRAW)
  glEnableVertexAttribArray(0)
  glVertexAttribPointer(0, 4, GL_FLOAT, false, 4 * 4, 0)
  glBindBuffer(GL_ARRAY_BUFFER, 0)
  glBindVertexArray(0)
}