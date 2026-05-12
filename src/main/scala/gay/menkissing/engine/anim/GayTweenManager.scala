package gay.menkissing.engine.anim

import gay.menkissing.engine.GayBasic
import gay.menkissing.engine.GayG

import collection.mutable
import scala.reflect.TypeTest
import monocle.*

object GayTweenManager extends GayBasic {
  visible = false
  GayG.plugins.addPlugin(this)
  GayG.signals.preStateSwitch.add(clear)

  private val _tweens = mutable.ArrayBuffer[GayTween]()

  def clear(): Unit =
    _tweens.foreach { tween =>
      tween.active = false
      // tween.destroy()
    }

  def add(tween: GayTween, start: Boolean = false): tween.type =
    if (tween == null)
      return null
    
    _tweens.append(tween)

    if (start)
      tween.start()
    tween

  def remove(tween: GayTween, destroy: Boolean = true): tween.type =
    if (tween == null)
      return null
    
    tween.active = false
    if (destroy)
      tween.destroy()
    
    _tweens.filterInPlace(_ != tween)
    tween

  def forEachTweenOf[T <: AnyRef](obj: T, func: GayTween => Unit)(using test: TypeTest[AnyRef, T]): Unit =
    // iterates backwards for removal
    var i = _tweens.length
    while {
      val x = i
      i -= 1
      x > 0
    } do {
      val tween = _tweens(i)
      if (tween.isTweenOf(obj))
        func(tween)
    }


  override def update(): Unit =
    val finishedTweens = mutable.ArrayBuffer.empty[GayTween]
    

    _tweens.foreach { tween =>
      if (tween.active) {
        tween.update()
        if (tween.finished) {
          finishedTweens.append(tween)
        }
      }
    }

    finishedTweens.foreach(_.finish())


}
