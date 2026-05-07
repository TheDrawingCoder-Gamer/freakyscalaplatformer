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

final case class AilmentMap[V]
( freeze: V,
  burn: V,
  shock: V,
  dizzy: V,
  sleep: V,
  forget: V,
  confuse: V,
  fear: V,
  rage: V,
  brainwash: V,
  despair: V,
  lightDeath: V,
  darkDeath: V
) {
  def apply(ailment: Ailment): V = {
    ailment match
      case Ailment.Freeze => freeze
      case Ailment.Burn => burn
      case Ailment.Shock => shock
      case Ailment.Dizzy => dizzy
      case Ailment.Sleep => sleep
      case Ailment.Forget => forget
      case Ailment.Confuse => confuse
      case Ailment.Fear => fear
      case Ailment.Rage => rage
      case Ailment.Brainwash => brainwash
      case Ailment.Despair => despair
      case Ailment.LightDeath => lightDeath
      case Ailment.DarkDeath => darkDeath
  }
  def updated(ailment: Ailment, value: V): AilmentMap[V] = {
    ailment match
      case Ailment.Freeze => copy(freeze = value)
      case Ailment.Burn => copy(burn = value)
      case Ailment.Shock => copy(shock = value)
      case Ailment.Dizzy => copy(dizzy = value)
      case Ailment.Sleep => copy(sleep = value)
      case Ailment.Forget => copy(forget = value)
      case Ailment.Confuse => copy(confuse = value)
      case Ailment.Fear => copy(fear = value)
      case Ailment.Rage => copy(rage = value)
      case Ailment.Brainwash => copy(brainwash = value)
      case Ailment.Despair => copy(despair = value)
      case Ailment.LightDeath => copy(lightDeath = value)
      case Ailment.DarkDeath => copy(darkDeath = value)
  }
}

final case class DefaultAilmentMap[V](default: V) {
  def apply
  ( freeze: V = default,
    burn: V = default,
    shock: V = default,
    dizzy: V = default,
    sleep: V = default,
    forget: V = default,
    confuse: V = default,
    fear: V = default,
    rage: V = default,
    brainwash: V = default,
    despair: V = default,
    lightDeath: V = default,
    darkDeath: V = default): AilmentMap[V] = AilmentMap(freeze, burn, shock, dizzy, sleep, forget, confuse, fear, rage, brainwash, despair, lightDeath, darkDeath)
}

val ResistAilmentMap = DefaultAilmentMap[AilmentResist](AilmentResist.Normal)
type ResistAilmentMap = AilmentMap[AilmentResist]