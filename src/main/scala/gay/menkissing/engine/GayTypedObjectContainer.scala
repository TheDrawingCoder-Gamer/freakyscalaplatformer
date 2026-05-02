package gay.menkissing.engine

import scala.collection.mutable

class GayTypedObjectContainer[T <: GayObject] extends GayTypedObjectGroup[T] {
  override val group: GayTypedContainer[T] = GayTypedObjectContainer.ObjectContainer[T](this)

  override def render(): Unit = {
    val oldDefaults = Game.instance.gamemanager.cameras.defaults
    if (cameras.cameras.nonEmpty || cameras.removeFromDefault) {
      val newBuf = if (cameras.removeFromDefault) mutable.Buffer.empty[GayView] else mutable.Buffer.from(oldDefaults)
      newBuf ++= cameras.cameras
      Game.instance.gamemanager.cameras.defaults = newBuf
    }
    
    super.render()
    
    Game.instance.gamemanager.cameras.defaults = oldDefaults
  }
  
  
}

object GayTypedObjectContainer {
  private class ObjectContainer[T <: GayObject](val parentSprite: GayTypedObjectContainer[T]) extends GayTypedContainer[T] {
    override def container: Option[GayContainer] = parentSprite.container

    override def cameras: SpriteCameras = parentSprite.cameras
  }
}