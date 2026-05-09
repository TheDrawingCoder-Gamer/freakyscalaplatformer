package gay.menkissing.battle

// This ordinal is also the ordering of the spritesheet's sprites
enum Element(val id: String, val physical: Boolean = false, val damage: Boolean = true) {
  case Strike extends Element(id = "strike", physical = true)
  case Slash extends Element(id = "slash", physical = true)
  case Pierce extends Element(id = "pierce", physical = true)

  case Fire extends Element(id = "fire")
  case Ice extends Element(id = "ice")

  case Wind extends Element(id = "wind")
  case Electric extends Element(id = "electric")

  case Psy extends Element(id = "psy")
  case Nuke extends Element(id = "nuke")

  case Bless extends Element(id = "bless")
  case Curse extends Element(id = "curse")

  case Almighty extends Element(id = "almighty")

  case Healing extends Element(id = "healing", damage = false)
  case Ailment extends Element(id = "ailment", damage = false)
  case Support extends Element(id = "support", damage = false)
  case Passive extends Element(id = "passive", damage = false)
}

object Element {
  val damageElements: List[Element] = Element.values.filter(_.damage).toList
}

type ElementMap[V] = Map[Element, V]

enum ResistLevel(val resistNumber: Option[Double] = None, val stopsDodge: Boolean = false) {
  case Weak extends ResistLevel(resistNumber = Some(2.0))
  case Normal extends ResistLevel(resistNumber = Some(1.0))
  case Resist extends ResistLevel(resistNumber = Some(0.5))
  case NullDamage extends ResistLevel(stopsDodge = true)
  case Drain extends ResistLevel(stopsDodge = true)
  case Reflect extends ResistLevel(stopsDodge = true)
}

enum SkillPotential(
                     val damageMultiplier: Double,
                     val healMultiplier: Option[Double],
                     val fullCostMultiplier: Double,
                     val shortCostMultiplier: Option[Double],
                     val ailmentMultiplier: Double) {
  case Down9 extends SkillPotential(0.45, None, 1.6, None, 0.4)
  case Down8 extends SkillPotential(0.53, None, 1.5, None, 0.5)
  case Down7 extends SkillPotential(0.57, None, 1.45, None, 0.55)
  case Down6 extends SkillPotential(0.61, None, 1.4, None, 0.6)
  case Down5 extends SkillPotential(0.65, Some(0.6), 1.35, Some(1.6), 0.65)
  case Down4 extends SkillPotential(0.75, Some(0.75), 1.25, Some(1.5), 0.75)
  case Down3 extends SkillPotential(0.8, Some(0.8), 1.2, Some(1.4), 0.8)
  case Down2 extends SkillPotential(0.85, Some(0.85), 1.15, Some(1.3), 0.85)
  case Down1 extends SkillPotential(0.9, Some(0.9), 1.1, Some(1.2), 0.9)
  case Neutral extends SkillPotential(1, Some(1), 1, Some(1), 1)
  case Up1 extends SkillPotential(1.1, Some(1.1), 0.9, Some(0.85), 1.1)
  case Up2 extends SkillPotential(1.15, Some(1.15), 0.87, Some(0.8), 1.15)
  case Up3 extends SkillPotential(1.2, Some(1.2), 0.84, Some(0.75), 1.20)
  case Up4 extends SkillPotential(1.25, Some(1.25), 0.81, Some(0.7), 1.25)
  case Up5 extends SkillPotential(1.35, Some(1.3), 0.75, Some(0.60), 1.35)
  case Up6 extends SkillPotential(1.39, None, 0.72, None, 1.4)
  case Up7 extends SkillPotential(1.43, None, 0.69, None, 1.45)
  case Up8 extends SkillPotential(1.47, None, 0.66, None, 1.5)
  case Up9 extends SkillPotential(1.55, None, 0.6, None, 1.6)
}


type ResistElementMap = ElementMap[ResistLevel]
object ResistElementMap {
  def apply(): ResistElementMap =
    Map.empty[Element, ResistLevel].withDefaultValue(ResistLevel.Normal)
  def apply(elements: (Element, ResistLevel)*): ResistElementMap =
    Map(elements*).withDefaultValue(ResistLevel.Normal)
}
type PotentialElementMap = ElementMap[SkillPotential]
object PotentialElementMap {
  def apply(): PotentialElementMap =
    Map.empty[Element,SkillPotential].withDefaultValue(SkillPotential.Neutral)
  def apply(elements: (Element, SkillPotential)*): PotentialElementMap =
    Map(elements*).withDefaultValue(SkillPotential.Neutral)
}