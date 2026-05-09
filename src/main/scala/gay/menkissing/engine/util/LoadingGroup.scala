package gay.menkissing.engine.util

import scala.collection.mutable
import java.io.Closeable

abstract class LoadingGroup[A <: Closeable] extends Closeable {
  private val members = mutable.Buffer.empty[A]

  extension (self: A)
    def register(): A =
      members.append(self)
      self
  
  def close(): Unit =
    members.foreach(_.close())
    members.clear()
}
