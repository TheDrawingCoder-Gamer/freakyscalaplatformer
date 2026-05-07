package gay.menkissing.engine.util

import scala.collection.mutable

trait GayTypedSignal[F] {
  def dispatch: F

  def add(listener: F): Unit
  def addOnce(listener: F): Unit
  def remove(listener: F): Unit
  def removeAll(): Unit
  def has(listener: F): Boolean
}

private class GaySignalHandler[F](val listener: F, val dispatchOnce: Boolean)

sealed abstract class GayBaseSignal[F] extends GayTypedSignal[F] {
  protected val handlers: mutable.ArrayBuffer[GaySignalHandler[F]] = mutable.ArrayBuffer.empty
  protected val pendingRemove: mutable.ArrayBuffer[GaySignalHandler[F]] = mutable.ArrayBuffer.empty
  protected var processingListeners: Boolean = false

  def add(listener: F): Unit =
    registerListener(listener, dispatchOnce = false)
  
  def addOnce(listener: F): Unit =
    registerListener(listener, dispatchOnce = true)

  def remove(listener: F): Unit =
    getHandler(listener).foreach(removeHandler)

  def removeAll(): Unit =
    handlers.clear()
    pendingRemove.clear()

  def has(listener: F): Boolean =
    getHandler(listener).isDefined

  protected def removeHandler(handler: GaySignalHandler[F]): Unit =
    if (processingListeners)
      pendingRemove.append(handler)
    else
      handlers.filterInPlace(_ != handler)

  protected def registerListener(listener: F, dispatchOnce: Boolean): GaySignalHandler[F] =
    getHandler(listener) match
      case Some(value) =>
        if (value.dispatchOnce != dispatchOnce)
          throw new IllegalStateException("You can't addOnce() then add() the same listener without removing the original listener first.")
        value
      case None =>
        val handler = new GaySignalHandler(listener, dispatchOnce)
        handlers.append(handler)
        handler
    

  protected def getHandler(listener: F): Option[GaySignalHandler[F]] =
    handlers.find(_.listener == listener)
}

// Flixel does this via macros but I dont really feel like busting
// out that shit rn
class GaySignal0 extends GayBaseSignal[() => Unit] {
  def dispatch: () => Unit = dispatch0

  def dispatch0(): Unit =
    processingListeners = true
    handlers.foreach { handler =>
      handler.listener()

      if (handler.dispatchOnce) {
        removeHandler(handler)
      }
    }

    processingListeners = false

    pendingRemove.foreach(removeHandler)
    pendingRemove.clear()
}

class GaySignal1[T1] extends GayBaseSignal[T1 => Unit] {
  def dispatch: T1 => Unit = dispatch1

  def dispatch1(v: T1): Unit =
    processingListeners = true
    handlers.foreach { handler => 
      handler.listener(v)

      if (handler.dispatchOnce) {
        removeHandler(handler)
      }
    }
    processingListeners = false

    pendingRemove.foreach(removeHandler)
    pendingRemove.clear()
}

class GaySignal2[T1, T2] extends GayBaseSignal[(T1, T2) => Unit] {
  def dispatch: (T1, T2) => Unit = dispatch2

  def dispatch2(a: T1, b: T2): Unit =
    processingListeners = true
    handlers.foreach { handler => 
      handler.listener(a, b)

      if (handler.dispatchOnce) {
        removeHandler(handler)
      }
    }
    processingListeners = false

    pendingRemove.foreach(removeHandler)
    pendingRemove.clear()
}

class GaySignal3[T1, T2, T3] extends GayBaseSignal[(T1, T2, T3) => Unit] {
  def dispatch: (T1, T2, T3) => Unit = dispatch3

  def dispatch3(a: T1, b: T2, c: T3): Unit =
    processingListeners = true
    handlers.foreach { handler => 
      handler.listener(a, b, c)

      if (handler.dispatchOnce) {
        removeHandler(handler)
      }
    }
    processingListeners = false

    pendingRemove.foreach(removeHandler)
    pendingRemove.clear()
}

class GaySignal4[T1, T2, T3, T4] extends GayBaseSignal[(T1, T2, T3, T4) => Unit] {
  def dispatch: (T1, T2, T3, T4) => Unit = dispatch4

  def dispatch4(a: T1, b: T2, c: T3, d: T4): Unit =
    processingListeners = true
    handlers.foreach { handler => 
      handler.listener(a, b, c, d)

      if (handler.dispatchOnce) {
        removeHandler(handler)
      }
    }
    processingListeners = false

    pendingRemove.foreach(removeHandler)
    pendingRemove.clear()
}