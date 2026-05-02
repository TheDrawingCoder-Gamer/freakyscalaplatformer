package gay.menkissing.engine.graphics

final class LoadTextureSlot[A <: TexturePack](val factory: () => A) {
  var data: Option[A] = None

  def get: A =
    data match
      case Some(x) => x
      case None =>
        val x = factory()
        data = Some(x)
        x

  def unload(): Unit =
    data.foreach(_.unload())
    data = None
}
