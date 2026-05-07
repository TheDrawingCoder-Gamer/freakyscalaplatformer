package gay.menkissing.engine.input
package action

class GayActionInput(
  val device: GayInputDevice,
  val deviceID: Int,
  val inputID: Int,
  val trigger: GayInputState,
  val kind: GayInputKind
)

object GayActionInput {
  def compareState(condition: GayInputState, state: GayInputState): Boolean =
    condition match
      case GayInputState.JustPressed =>
        state match
          case GayInputState.JustPressed => true
          case _ => false
      case GayInputState.JustReleased =>
        state match
          case GayInputState.JustReleased => true
          case _ => false
      case GayInputState.Pressed =>
        state match
          case GayInputState.JustPressed | GayInputState.Pressed => true
          case _ => false
      case GayInputState.Released =>
        state match
          case GayInputState.JustReleased | GayInputState.Released => true
          case _ => false
    
}