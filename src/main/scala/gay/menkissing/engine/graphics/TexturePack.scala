package gay.menkissing.engine.graphics

import gay.menkissing.draw.Texture
import scala.collection.mutable

abstract class TexturePack {
  private val textures = mutable.Buffer[Texture]()
  
  extension (texture: Texture)
    def register(): Texture =
      textures.append(texture)
      texture
  
  
  def unload(): Unit =
    textures.foreach(_.close())
    textures.clear()
}
