package gay.menkissing
package battle

import common.math as gaymath

enum AilmentResult {
  case Ok
  case Dead
}
enum AilmentResist(val multiplier: Double) {
  case Normal extends AilmentResist(multiplier = 1.0)
  case Resist extends AilmentResist(multiplier = 0.5)
  case NullAilment extends AilmentResist(multiplier = 0.0)
}
enum Ailment(val id: String, val technicals: List[Element] = List(), val actionable: Boolean = true, val expireTimer: Range, val associatedWith: Option[Element] = None) {
  case Freeze extends Ailment(id = "freeze", technicals = List(Element.Nuke, Element.Pierce, Element.Strike, Element.Slash), actionable =  false, expireTimer = 1 to 1, associatedWith = Some(Element.Ice))
  case Burn extends Ailment(id = "burn", technicals = List(Element.Nuke, Element.Wind), expireTimer = 2 to 4, associatedWith = Some(Element.Fire))
  case Shock extends Ailment(id = "shock", technicals = List(Element.Nuke, Element.Pierce, Element.Strike, Element.Slash), actionable = false, expireTimer = 1 to 1, associatedWith = Some(Element.Electric))

  case Dizzy extends Ailment(id = "dizzy", technicals = Element.damageElements, expireTimer = 1 to 2)
  case Sleep extends Ailment(id = "sleep", technicals = Element.damageElements, actionable = false, expireTimer = 5 to 10)

  case Forget extends Ailment(id = "forget", technicals = List(Element.Psy, Element.Electric), expireTimer = 3 to 5)
  case Confuse extends Ailment(id = "confuse", technicals = List(Element.Psy, Element.Wind, Element.Pierce), actionable = false, expireTimer = 2 to 4)
  case Fear extends Ailment(id = "fear", technicals = List(Element.Psy, Element.Ice), expireTimer = 2 to 4)
  case Rage extends Ailment(id = "rage", technicals = List(Element.Psy, Element.Fire), actionable = false, expireTimer = 2 to 4)
  case Brainwash extends Ailment(id = "brainwash", technicals = List(Element.Psy, Element.Bless), actionable = false, expireTimer = 1 to 4)
  case Despair extends Ailment(id = "despair", technicals = List(Element.Psy, Element.Curse), expireTimer = 1 to 4)

  // ???
  // These aren't ever actually applied, and just kill upon application.
  // They are defined as ailments so bosses, for example, can defend against them
  // explicitly without having to resist light or dark
  case LightDeath extends Ailment(id = "lightdeath", actionable = false, expireTimer = 0 to 0, associatedWith = Some(Element.Bless))
  case DarkDeath extends Ailment(id = "darkdeath", actionable = false, expireTimer = 0 to 0, associatedWith = Some(Element.Curse))

}

type AilmentMap[V] = Map[Ailment, V]



type ResistAilmentMap = AilmentMap[AilmentResist]
object ResistAilmentMap {
  def apply(kv: (Ailment, AilmentResist)*): ResistAilmentMap =
    Map(kv*).withDefaultValue(AilmentResist.Normal)
}