package gay.menkissing.engine
package group


class GayTypedContainer[T <: GayBasic] extends GayTypedGroup[T] {
  override def onMemberAdded(member: T): Unit =
    member.container.foreach(_.remove(member))
    member.container = Some(this.asInstanceOf[GayTypedContainer[GayBasic]])
    super.onMemberAdded(member)

  override def onMemberRemove(member: T): Unit =
    member.container = None
    super.onMemberRemove(member)
}

type GayContainer = GayTypedContainer[GayBasic]