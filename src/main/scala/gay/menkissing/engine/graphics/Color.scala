package gay.menkissing.engine.graphics

import org.lwjgl.opengl.GL20.glUniform4f

final case class Color(r: Float, g: Float, b: Float, a: Float) {
  def bind(targetUL: Int): Unit =
    glUniform4f(targetUL, r, g, b, a)
}

object Color {
  def fromRGBA8(r: Int, g: Int, b: Int, a: Int) = new Color(r.toFloat / 255, g.toFloat / 255, b.toFloat / 255, a
    .toFloat / 255)

  def fromHex(hex: Int): Color = {
    val r = (hex & 0x00FF0000) >>> 16
    val g = (hex & 0x0000FF00) >>> 8
    val b = (hex & 0x000000FF)
    val a = (hex & 0xFF000000) >>> 24
    fromRGBA8(r, g, b, a)

  }
}