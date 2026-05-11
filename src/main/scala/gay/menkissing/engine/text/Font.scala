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
import gay.menkissing.draw.TextRendering
import upickle.default.*
import org.joml.Vector3f
import gay.menkissing.draw.Shaders
import gay.menkissing.engine.text.Font.FontKind

class Font(val texture: Texture, val atlas: FontAtlas, val overrideProgram: Option[Int] = None) {
  private def program: Int =
    overrideProgram.getOrElse(atlas.atlas.kind.program)
  private def getScreenScale(textHeight: Int): Float =
    val glyph = atlas.glyphs.find(_._2.texInfo.isDefined).get._2.texInfo.get
    val inputSizePx = glyph.atlasBounds.w
    val outputSizePx = glyph.planeBounds.w * GayG.currentCamera.width
    (outputSizePx / inputSizePx).toFloat


  def drawString(text: String, color: Color, matrices: Matrix4f, textSize: Int): Unit =
    glUseProgram(atlas.atlas.kind.program)
    val projectionLocation = glGetUniformLocation(program, "projection")
    Using.resource(stackPush()) { stack =>
      val fb = stack.mallocFloat(16)
      glUniformMatrix4fv(projectionLocation, false, matrices.get(fb))
    }

    val colorLoc = glGetUniformLocation(program, "textColor")
    color.bind(colorLoc)
    if (atlas.atlas.kind.isSDF) {
      val inverseWidthLoc = glGetUniformLocation(atlas.atlas.kind.program, "inverseWidth")
      val antialiasPerEm = atlas.atlas.size / (Game.windowWidth.toDouble / GayG.currentCamera.width)
      val scale = getScreenScale(textSize)
      // println(scale)
      glUniform1f(inverseWidthLoc, (scale * antialiasPerEm).toFloat)
      val aemRangeLocation = glGetUniformLocation(atlas.atlas.kind.program, "aemRange")
      val aemRange = atlas.atlas.aemRange.get
      glUniform2f(aemRangeLocation, aemRange.low.toFloat, aemRange.high.toFloat)
      val thresholdLocation = glGetUniformLocation(atlas.atlas.kind.program, "thresholdEm")
      glUniform1f(thresholdLocation, 0.0)
    }

    
    glActiveTexture(GL_TEXTURE0)
    glBindVertexArray(TextRendering.textVAO)
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

            glBindBuffer(GL_ARRAY_BUFFER, TextRendering.textVBO)
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
      

    def program: Int =
      this match
        // Ignore the true SDF in the MTSDF
        case MSDF | MTSDF => Shaders.fontMSDFProgram
        case SDF => Shaders.fontTSDFProgram
        case SoftMask => Shaders.fontSoftMaskProgram
      
  }

  def load(texPath: String, atlasPath: String): Font =
    val texture = Texture(getClass.getResourceAsStream(texPath), GL_NEAREST, GL_NEAREST, Texture.RenderMode.Blend, false)
    val atlas = ujson.InputStreamParser.transform[FontAtlas](getClass.getResourceAsStream(atlasPath), upickle.default.reader[FontAtlas])
    Font(texture, atlas)

  lazy val defaultFont: Font = Font.load("/fonts/default/font.png", "/fonts/default/font.json")
}