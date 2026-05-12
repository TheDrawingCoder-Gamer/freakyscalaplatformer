package gay.menkissing.engine.anim

import monocle.*
import scala.reflect.TypeTest
import collection.mutable

class GayTween(val duration: Int, val ease: Double => Double = identity) {
   // var percent: Double = 0.0
  var scale: Double = 0.0


  var active: Boolean = false

  private var _running = false

  private var _framesSinceStart: Int = 0

  private val _chainedTweens = mutable.ArrayBuffer.empty[GayTween]
  private var _nextTweenInChain: GayTween = null

  var delay: Int = 0

  private var _finished: Boolean = false

  def finished: Boolean = _finished

  def isTweenOf(obj: AnyRef): Boolean = false

  def start(): this.type =
    _framesSinceStart = 0
    active = true
    _running = true
    _finished = false
    this

  private def setVarsOnEnd(): Unit =
    active = false
    _running = false
    _finished = true

  private def onEnd(): Unit =
    setVarsOnEnd()
    processTweenChain()

  def andThen(that: GayTween): this.type =
    addChainedTween(that)

  def cancel(): Unit =
    onEnd()

    GayTweenManager.remove(this)

  def cancelChain(): Unit =
    if (_nextTweenInChain != null)
      _nextTweenInChain.cancelChain()
    
    _chainedTweens.clear()

    cancel()

  def wait(delay: Int): this.type =
    addChainedTween(DelayTween(delay))

  def finish(): Unit =
    onEnd()

    GayTweenManager.remove(this)

  private def setChain(previousChain: mutable.ArrayBuffer[GayTween]): Unit =
    _chainedTweens.appendAll(previousChain)

  private def processTweenChain(): Unit =
    if (_chainedTweens.isEmpty)
      return
    
    _nextTweenInChain = _chainedTweens.head
    _chainedTweens.dropInPlace(1)

    doNextTween(_nextTweenInChain)
    _chainedTweens.clear()

  private def doNextTween(tween: GayTween): Unit =
    if (!tween.active) {
      tween.start()
      GayTweenManager.add(tween)
    }
    tween.setChain(_chainedTweens)

  private def addChainedTween(tween: GayTween): this.type =
    tween.setVarsOnEnd()
    GayTweenManager.remove(tween, destroy = false)

    _chainedTweens.append(tween)
    this



  def update(): Unit =
    _framesSinceStart += 1
  
    scale = math.max((_framesSinceStart - delay).toDouble, 0.0) / duration.toDouble
    scale = ease(scale)


    if (_framesSinceStart > delay && !_running) {
      _running = true
    }

    if (_framesSinceStart >= duration + delay) {
      scale = 1.0
      _finished = true
    }


  def destroy(): Unit = ()
}

object GayTween {
  def tween[O <: AnyRef, F](obj: O, accessor: TweenAccessor[O, F], result: F, duration: Int, ease: Double => Double = identity, start: Boolean = false)(using tweenable: GayTweenable[F]): GayTween =
    val r = new VarGayTween[O, F](obj, accessor, result, duration, ease)
    GayTweenManager.add(r, start = start)
}

class DelayTween(duration: Int) extends GayTween(duration)

class VarGayTween[O <: AnyRef, F](val obj: O, val accessor: TweenAccessor[O, F], val result: F, duration: Int, ease: Double => Double = identity)(using tweenable: GayTweenable[F]) extends GayTween(duration, ease ){


  var startPos: F = accessor.get(obj)

  override def isTweenOf(obj: AnyRef): Boolean =
    obj eq this.obj
  override def update(): Unit = 
    super.update()

    accessor.update(obj, tweenable.tween(startPos, result, scale))


}