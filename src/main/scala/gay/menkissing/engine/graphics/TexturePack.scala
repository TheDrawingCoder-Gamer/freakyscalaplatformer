package gay.menkissing.engine.graphics

import scala.collection.mutable
import java.io.Closeable

abstract class TexturePack extends Closeable {
  private val textures = mutable.Buffer[Texture]()
  
  extension (texture: Texture)
    def register(): Texture =
      textures.append(texture)
      texture
  
  
  def close(): Unit =
    textures.foreach(_.close())
    textures.clear()
}
