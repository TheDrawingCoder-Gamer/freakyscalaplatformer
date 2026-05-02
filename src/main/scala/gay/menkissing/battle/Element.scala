package gay.menkissing.battle

enum Element(val name: String, val physical: Boolean = false, val damage: Boolean = true) {
  case Strike extends Element(name = "Strike", physical = true)
  case Slash extends Element(name = "Slash", physical = true)
  case Pierce extends Element(name = "Pierce", physical = true)

  case Fire extends Element(name = "Fire")
  case Ice extends Element(name = "Ice")

  case Wind extends Element(name = "Wind")
  case Electric extends Element(name = "Electric")

  case Psy extends Element(name = "Psy")
  case Nuke extends Element(name = "Nuke")

  case Light extends Element(name = "Light")
  case Dark extends Element(name = "Dark")

  case Almighty extends Element(name = "Almighty")

  case Healing extends Element(name = "Healing", damage = false)
  case Ailment extends Element(name = "Ailment", damage = false)
}

object Element {
  val damageElements: List[Element] = Element.values.filter(_.damage).toList
}

final case class DamageElementMap[V]
( strike: V,
  slash: V,
  pierce: V,
  fire: V,
  ice: V,
  wind: V,
  electric: V,
  psy: V,
  nuke: V,
  light: V,
  dark: V,
  almighty: V,
) {
  def get(key: Element): Option[V] = {
    key match
      case Element.Strike => Some(strike)
      case Element.Slash => Some(slash)
      case Element.Pierce => Some(pierce)
      case Element.Fire => Some(fire)
      case Element.Ice => Some(ice)
      case Element.Wind => Some(wind)
      case Element.Electric => Some(electric)
      case Element.Psy => Some(psy)
      case Element.Nuke => Some(nuke)
      case Element.Light => Some(light)
      case Element.Dark => Some(dark)
      case Element.Almighty => Some(almighty)
      case _ => None
  }
  def updated(key: Element, v: V): DamageElementMap[V] = {
    key match
      case Element.Strike => this.copy(strike = v)
      case Element.Slash => this.copy(slash = v)
      case Element.Pierce => this.copy(pierce = v)
      case Element.Fire => this.copy(fire = v)
      case Element.Ice => this.copy(ice = v)
      case Element.Wind => this.copy(wind = v)
      case Element.Electric => this.copy(electric = v)
      case Element.Psy => this.copy(psy = v)
      case Element.Nuke => this.copy(nuke = v)
      case Element.Light => this.copy(light = v)
      case Element.Dark => this.copy(dark = v)
      case Element.Almighty => this.copy(almighty = v)
      case _ => this
  }
}

final case class DefaultDamageElementMapFactory[V](default: V) {
  def apply(strike: V = default,
            slash: V = default,
            pierce: V = default,
            fire: V = default,
            ice: V = default,
            wind: V = default,
            electric: V = default,
            psy: V = default,
            nuke: V = default,
            light: V = default,
            dark: V = default,
            almighty: V = default
           ): DamageElementMap[V] = DamageElementMap[V](strike, slash, pierce, fire, ice, wind, electric, psy, nuke, light, dark, almighty)
}

enum ResistLevel(val resistNumber: Option[Double] = None, val stopsDodge: Boolean = false) {
  case Weak extends ResistLevel(resistNumber = Some(2.0))
  case Normal extends ResistLevel(resistNumber = Some(1.0))
  case Resist extends ResistLevel(resistNumber = Some(0.5))
  case NullDamage extends ResistLevel(stopsDodge = true)
  case Drain extends ResistLevel(stopsDodge = true)
  case Reflect extends ResistLevel(stopsDodge = true)
}



val ResistElementMap = DefaultDamageElementMapFactory[ResistLevel](ResistLevel.Normal)
type ResistElementMap = DamageElementMap[ResistLevel]