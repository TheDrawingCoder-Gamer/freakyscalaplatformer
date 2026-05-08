package gay.menkissing.engine.graphics

import gay.menkissing.draw.Texture
import scala.collection.mutable
import gay.menkissing.engine.util.GayDestroyable

abstract class TexturePack extends GayDestroyable {
  private val textures = mutable.Buffer[Texture]()
  
  extension (texture: Texture)
    def register(): Texture =
      textures.append(texture)
      texture
  
  
  def destroy(): Unit =
    textures.foreach(_.close())
    textures.clear()
}
