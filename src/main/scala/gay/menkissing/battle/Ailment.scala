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
enum Ailment(val technicals: List[Element] = List(), val actionable: Boolean = true, val expireTimer: Range, val associatedWith: Option[Element] = None) {
  case Freeze extends Ailment(technicals = List(Element.Nuke, Element.Pierce, Element.Strike, Element.Slash), actionable =  false, expireTimer = 1 to 1, associatedWith = Some(Element.Ice))
  case Burn extends Ailment(technicals = List(Element.Nuke, Element.Wind), expireTimer = 2 to 4, associatedWith = Some(Element.Fire))
  case Shock extends Ailment(technicals = List(Element.Nuke, Element.Pierce, Element.Strike, Element.Slash), actionable = false, expireTimer = 1 to 1, associatedWith = Some(Element.Electric))

  case Dizzy extends Ailment(technicals = Element.damageElements, expireTimer = 1 to 2)
  case Sleep extends Ailment(technicals = Element.damageElements, actionable = false, expireTimer = 5 to 10)

  case Forget extends Ailment(technicals = List(Element.Psy, Element.Electric), expireTimer = 3 to 5)
  case Confuse extends Ailment(technicals = List(Element.Psy, Element.Wind, Element.Pierce), actionable = false, expireTimer = 2 to 4)
  case Fear extends Ailment(technicals = List(Element.Psy, Element.Ice), expireTimer = 2 to 4)
  case Rage extends Ailment(technicals = List(Element.Psy, Element.Fire), actionable = false, expireTimer = 2 to 4)
  case Brainwash extends Ailment(technicals = List(Element.Psy, Element.Light), actionable = false, expireTimer = 1 to 4)

  // ???
  case LightDeath extends Ailment(actionable = false, expireTimer = 0 to 0, associatedWith = Some(Element.Light))
  case DarkDeath extends Ailment(actionable = false, expireTimer = 0 to 0, associatedWith = Some(Element.Dark))

  def processApplication(battle: Battle, target: SidedFighter): Unit = {

    if (target.fighter.curAilment.isEmpty) {
      if (this != LightDeath && this != DarkDeath) {
        println(s"${target.fighter.name} got inflicted with $this")
      }
      target.fighter.curAilment = Some(this)
      target.fighter.ailmentTimer = battle.randomIn(this.expireTimer)
    }
    this match {
      case Ailment.LightDeath | Ailment.DarkDeath => {
        target.fighter.health = 0
        target.fighter.curAilment = None
        battle.processKill(target)
      }
      case _ => ()
    }
  }
  def canPerformAction(battle: Battle, skill: Skill): Boolean = {
    this.actionable && (this != Ailment.Forget || !skill.personaSkill)
  }
  def withAction(battle: Battle, action: Option[QueuedAction], fighter: SidedFighter)(using Events): AilmentResult = {
    this match
      case Ailment.Burn => {
        action.foreach(it => battle.runAction(it, fighter))
        val dmgAmount = fighter.fighter.maxHP / 10
        fighter.fighter.health -= dmgAmount
        if (fighter.fighter.health < 1) fighter.fighter.health = 1
        println(s"${fighter.fighter.name} was burned for $dmgAmount!")
        AilmentResult.Ok
      }
      case Ailment.Sleep => {
        println(s"${fighter.fighter.name} is fast asleep...")
        val hpHeal = fighter.fighter.maxHP / 10
        val spHeal = fighter.fighter.maxSP / 10
        println(s"${fighter.fighter.name} healed $hpHeal HP and $spHeal SP")
        fighter.fighter.health += hpHeal
        fighter.fighter.verifyHealth()
        fighter.fighter.verifySP()
        AilmentResult.Ok
      }
      // TODO: confuse
      case Ailment.Confuse => {
        println(s"${fighter.fighter.name} is confused, but did nothing because this isn't implemented yet")
        AilmentResult.Ok
      }
      case Ailment.Fear => {
        val actionWorked = battle.randomIn(0 to 5) > 3
        if (actionWorked) {
          action.foreach(it => battle.runAction(it, fighter))
        } else {
          println(s"${fighter.fighter.name} was too scared to fight...")
        }
        AilmentResult.Ok
      }
      case Ailment.Rage => {
        val enemy = battle.selectTargetRandom(!fighter.allied)
        battle.processSkill(fighter.fighter.basicAttackSkill, fighter, enemy)
        AilmentResult.Ok
      }
      case Ailment.Brainwash => {
        val availableSkills = fighter.fighter.skills.filter(it => it.canAffordSkill(fighter.fighter, fighter.allied) && it.target.negate != it.target)
        val skill = battle.randomFrom(availableSkills.toArray)
        if (skill.target.multiTarget) {
          // negation
          val allySide = (skill.target == SkillTarget.AllAllies) ^ fighter.allied
          battle.processMultiSkill(skill, fighter, battle.selectSide(allySide).filter(_ != fighter))
        } else {
          val allySide = (skill.target == SkillTarget.Ally) ^ fighter.allied
          battle.processSkill(skill, fighter, battle.selectTargetRandom(allySide))
        }
        AilmentResult.Ok
      }
      case _ => {
        action.foreach(it => battle.runAction(it, fighter))
        AilmentResult.Ok
      }
  }
}

case class AilmentMap[V]
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
      case Ailment.LightDeath => copy(lightDeath = value)
      case Ailment.DarkDeath => copy(darkDeath = value)
  }
}

case class DefaultAilmentMap[V](default: V) {
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
    lightDeath: V = default,
    darkDeath: V = default): AilmentMap[V] = AilmentMap(freeze, burn, shock, dizzy, sleep, forget, confuse, fear, rage, brainwash, lightDeath, darkDeath)
}

val ResistAilmentMap = DefaultAilmentMap[AilmentResist](AilmentResist.Normal)
type ResistAilmentMap = AilmentMap[AilmentResist]