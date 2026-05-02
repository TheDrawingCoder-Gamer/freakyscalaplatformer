package gay.menkissing.engine

import scala.collection.mutable

class SpriteCameras {
  val cameras = mutable.Set[GayView]()
  var removeFromDefault = false

  def hideFromDefault(): this.type =
    this.removeFromDefault = true
    this
    
  def showOnDefault(): this.type =
    this.removeFromDefault = false
    this
  
  def addToCamera(cam: GayView): this.type =
    this.cameras.add(cam)
    this
  
  def removeFromCamera(cam: GayView): this.type =
    this.cameras.remove(cam)
    this
  
  def reset(): Unit =
    cameras.clear()
    removeFromDefault = false
}
