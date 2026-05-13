package gay.menkissing.dialog

/**
 * An actor that is controllable in cutscenes.
 */
trait CutControllable {
  /**
    * The ID of this actor, for events to reference
    */
  val actorID: String
}
