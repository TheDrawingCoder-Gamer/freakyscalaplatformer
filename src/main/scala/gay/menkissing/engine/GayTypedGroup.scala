package gay.menkissing.engine

import gay.menkissing.draw.MatrixStack

import collection.mutable

class GayTypedGroup[T <: GayBasic] extends GayBasic {
  protected val members = mutable.ArrayBuffer[T]()

  def length: Int = members.length

  def add(basic: T): basic.type =
    members.append(basic)
    onMemberAdded(basic)
    basic
  
  def remove(basic: T): basic.type =
    members.filterInPlace(_ != basic)
    onMemberRemove(basic)
    basic

  inline def exists(f: T => Boolean): Boolean = members.exists(f)

  inline def foreach(f: T => Unit): Unit = members.foreach(f)

  inline def foreachExists(f: T => Unit): Unit =
    members.withFilter(_.exists).foreach(f)

  override def update(): Unit =
    members.withFilter(i => i.exists && i.active).foreach(_.update())
    members.filterInPlace(!_.destroyed)

  override def render(): Unit =
    members.withFilter(i => i.exists && i.visible).foreach(_.render())


  def onMemberAdded(member: T): Unit = ()

  def onMemberRemove(member: T): Unit = ()

}
