package gay.menkissing.engine.util

/**
  * Used for loading shared objects like texture and audio packs
  *
  * @param factory
  */
final class LoadSlot[A <: GayDestroyable](val factory: () => A) {
  var data: Option[A] = None

  def get: A =
    data match
      case Some(x) => x
      case None =>
        val x = factory()
        data = Some(x)
        x

  def unload(): Unit =
    data.foreach(_.destroy())
    data = None
}
