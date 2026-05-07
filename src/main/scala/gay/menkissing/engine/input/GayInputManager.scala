package gay.menkissing.engine.input

trait GayInputManager {
  def update(): Unit
  def onFocus(): Unit
  def onFocusLost(): Unit
}
