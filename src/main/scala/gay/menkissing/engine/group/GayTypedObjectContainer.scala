package gay.menkissing.engine
package group

import scala.collection.mutable

open class GayTypedObjectContainer[T <: GayObject] extends GayTypedObjectGroup[T] {
  override val group: GayTypedContainer[T] = GayTypedObjectContainer.ObjectContainer[T](this)


  override def collectForRender(): Unit =
    val oldDefaults = Game.instance.gamemanager.cameras.defaultCam
    _layer.foreach { it =>
      Game.instance.gamemanager.cameras.defaultCam = it
    }
    
    super.collectForRender()
    
    Game.instance.gamemanager.cameras.defaultCam = oldDefaults
  
  
}

object GayTypedObjectContainer {
  private class ObjectContainer[T <: GayObject](val parentSprite: GayTypedObjectContainer[T]) extends GayTypedContainer[T] {
    override def container: Option[GayContainer] = parentSprite.container

    override def layer: GayView = parentSprite.layer
    override def setLayer(v: GayView | Null): Unit = parentSprite.setLayer(v)
  }
}

type GayObjectContainer = GayTypedObjectContainer[GayObject]