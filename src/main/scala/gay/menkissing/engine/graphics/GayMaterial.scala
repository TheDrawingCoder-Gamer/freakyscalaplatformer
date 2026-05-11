package gay.menkissing
package engine.graphics

import gay.menkissing.common.math as gaymath
import org.joml.Matrix4f
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL13.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*

sealed trait GayMaterial {
  def bind(transform: Matrix4f): Unit
}

object GayMaterial {
  final case class FlatColor(color: Color) extends GayMaterial {
    def bind(transform: Matrix4f): Unit =
      draw.setSolidColor(color, transform)
  }
  final class Tex2D(val texture: Texture, var uv: gaymath.Rect) extends GayMaterial {
    def bind(transform: Matrix4f): Unit =
      texture.bind()
      val texTransform = texture.texTransform(uv.x, uv.y, uv.w, uv.h)
      texture.renderMode.program.bindTransform(transform, texTransform)
  }

  object Tex2D {
    def apply(texture: Texture): Tex2D =
      val rect = gaymath.Rect(0, 0, texture.width, texture.height)
      new Tex2D(texture, rect)
  }
}
