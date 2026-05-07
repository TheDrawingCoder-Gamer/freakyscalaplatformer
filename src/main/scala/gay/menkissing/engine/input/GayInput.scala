package gay.menkissing.engine.input

class GayInput[T](var id: T) {
  var current: GayInputState = GayInputState.Released
  var last: GayInputState = GayInputState.Released
  def pressed: Boolean =
    current match
      case GayInputState.JustPressed | GayInputState.Pressed => true
      case _ => false
  def justPressed: Boolean = current == GayInputState.JustPressed
  def released: Boolean =
    current match
      case GayInputState.JustReleased | GayInputState.Released => true
      case _ => false
  def justReleased: Boolean = current == GayInputState.JustReleased
  
  def press(): Unit =
    last = current
    current = if (pressed) GayInputState.Pressed else GayInputState.JustPressed
  
  def release(): Unit =
    last = current
    current = if (pressed) GayInputState.JustReleased else GayInputState.Released

  def update(): Unit =
    if (last == GayInputState.JustReleased && current == GayInputState.JustReleased) {
      current = GayInputState.Released
    } else if (last == GayInputState.JustPressed && current == GayInputState.JustPressed) {
      current = GayInputState.Pressed
    }
    last = current

  def reset(): Unit =
    current = GayInputState.Released
    last = GayInputState.Released

  def hasState(state: GayInputState): Boolean =
    state match
      case GayInputState.JustPressed => justPressed
      case GayInputState.JustReleased => justReleased
      case GayInputState.Pressed => pressed
      case GayInputState.Released => released
    
}
