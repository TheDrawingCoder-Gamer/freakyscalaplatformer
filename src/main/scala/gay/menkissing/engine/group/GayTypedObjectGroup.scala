package gay.menkissing.engine
package group

// TODO: collisions (Im not doing that :joy:)
open class GayTypedObjectGroup[T <: GayObject] extends GayObject {
  val group = GayTypedGroup[T]()

    

  override def graphicalWidth: Int =
    val minX = group.members.minByOption(_.x).map(_.x).getOrElse(0)
    val maxX = group.members.maxByOption(o => o.x + o.graphicalWidth).map(o => o.x + o.graphicalWidth).getOrElse(0)
    maxX - minX
  override def graphicalHeight: Int =
    val minY = group.members.minByOption(_.y).map(_.y).getOrElse(0)
    val maxY = group.members.maxByOption(o => o.y + o.graphicalHeight).map(o => o.y + o.graphicalHeight).getOrElse(0)
    maxY - minY

  override def x_=(v: Int): Unit =
    if (exists && _x != v) {
      transformChildren(xTransform, v - _x)
    }
    _x = v

  override def y_=(v: Int): Unit =
    if (exists && _y != v) {
      transformChildren(yTransform, v - _y)
    }
    _y = v

  private def xTransform(s: T, v: Int): Unit =
    s.x += v
  private def yTransform(s: T, v: Int): Unit =
    s.y += v

  def transformChildren[V](f: (T, V) => Unit, value: V): Unit =
    group.foreach { x =>
      f(x, value)
    }

  def multiTransformChildren[V](fs: Vector[(T, V) => Unit], values: Vector[V]): Unit =
    val numProps = fs.length
    if (fs.sizeCompare(values) > 0)
      return
    val zipped = fs.zip(values)

    group.foreachExists { sprite =>
      zipped.foreach { (f, v) =>
        f(sprite, v)
      }
    }


  private def preAdd(sprite: T): Unit = {
    sprite.x += x
    sprite.y += y
    sprite.scrollFactor = this.scrollFactor
    sprite.cameras.reset()
    sprite.cameras.removeFromDefault = this.cameras.removeFromDefault
    sprite.cameras.cameras.addAll(this.cameras.cameras)
  }

  def add(sprite: T): sprite.type =
    preAdd(sprite)
    group.add(sprite)

  def setPosition(ix: Int, iy: Int): Unit =
    val dx = ix - x
    val dy = iy - y
    multiTransformChildren(Vector(xTransform, yTransform), Vector(dx, dy))

    _x = ix
    _y = iy

  def remove(sprite: T): sprite.type =
    sprite.x -= x
    sprite.y -= y
    sprite.cameras.reset()
    group.remove(sprite)

  override def render(): Unit =
    group.render()
}

type GayObjectGroup = GayTypedObjectGroup[GayObject]
