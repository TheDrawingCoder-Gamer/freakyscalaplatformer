package gay.menkissing.engine.util

import java.io.Closeable

/**
  * Used for loading shared objects like texture and audio packs
  *
  * @param factory
  */
final class LoadSlot[A <: Closeable](val factory: () => A) {
  var data: Option[A] = None

  def get: A =
    data match
      case Some(x) => x
      case None =>
        val x = factory()
        data = Some(x)
        x

  def unload(): Unit =
    data.foreach(_.close())
    data = None
}
