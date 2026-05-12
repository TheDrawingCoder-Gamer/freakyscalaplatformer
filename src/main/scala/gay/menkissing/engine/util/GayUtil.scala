package gay.menkissing.engine.util

import collection.mutable

object GayUtil {
  def insertSafe[A](buffer: mutable.Buffer[A], obj: A, at: Int): Unit =
    if (at >= buffer.length)
      buffer.append(obj)
    else
      val pos =
        if (at < 0)
          at + buffer.length
        else
          at
      
      buffer.insert(pos, obj)
}
