package gay.menkissing
package battle

import SkillCost.RealSkillCost

import scala.math.Ordering.Double.IeeeOrdering
import scala.util.boundary
import common.math as gaymath
import gay.menkissing.engine.L10n
import gay.menkissing.common.PRNG

enum SkillTarget(val multiTarget: Boolean = false) {
  case Self
  case Ally
  case AllAllies extends SkillTarget(multiTarget = true)
  case Foe
  case AllFoes extends SkillTarget(multiTarget = true)
  case All extends SkillTarget(multiTarget = true)

  def negate: SkillTarget =
    this match {
      case SkillTarget.Self => SkillTarget.Self
      case SkillTarget.Ally => SkillTarget.Foe
      case SkillTarget.AllAllies => SkillTarget.AllFoes
      case SkillTarget.Foe => SkillTarget.Ally
      case SkillTarget.AllFoes => SkillTarget.AllAllies
      case SkillTarget.All => SkillTarget.All
    }
}

enum SkillResult {
  case Missed
  // Remove once finished!!!
  case Unknown
}

enum SkillEffect {
  case Damage(power: Int)
  case Healing(percent: Double, power: Int)
  case ReduceHPTo1
  case ExactDamage(by: Int)
  case ExactHealing(by: Int)
  case HealAllHP
  case RevivePercent(percent: Int)
  case DamagePercentRemaining(percent: Int)
  case DamagePercentFull(percent: Int)
  case ApplyAilment(ailment: Ailment, rate: Int)
  case CureStatus
  case Group(ls: List[SkillEffect])
  // everything is defined in flags
  case None
}


enum SkillCost {
  case HP(percent: Int)
  case FlatHP(by: Int)
  case SPPercent(percent: Int)
  case SP(by: Int)

  def skillCost(caster: Fighter): SkillCost.RealSkillCost = {
    this match
      case SkillCost.HP(percent) => SkillCost.RealSkillCost.HP((caster.maxHP.toDouble * (percent.toDouble / 100)).toInt)
      case SkillCost.FlatHP(by) => SkillCost.RealSkillCost.HP(by)
      case SkillCost.SPPercent(percent) => SkillCost.RealSkillCost.SP((caster.maxSP.toDouble * (percent.toDouble / 100)).toInt)
      case SkillCost.SP(by) => SkillCost.RealSkillCost.SP(by)
  }
}
object SkillCost {
  enum RealSkillCost {
    case HP(by: Int)
    case SP(by: Int)

    override def toString: String =
      this match {
        case RealSkillCost.HP(by) => s"${by} HP"
        case RealSkillCost.SP(by) => s"${by} SP"
      }
  }
}

final case class SkillAilmentInfo(ailment: Ailment, ailmentRate: Int)

object Skill {
  val maxAccuracy: Int = 255
  
  def damagingAilment(
    id: String,
    basePower: Int,
    element: Element,
    accuracy: Int,
    cost: SkillCost,
    target: SkillTarget,
    ailment: SkillAilmentInfo,
    strengthBased: Boolean = false,
    personaSkill: Boolean = true,
    criticalRate: Int = 0,
    buffDebuffs: AppliedBuffs = AppliedBuffs.empty,
    specialFlags: SpecialSkillFlags = SpecialSkillFlags.empty,
    hits: Range = 1 to 1
  ): Skill =
    val damageEff = SkillEffect.Damage(basePower)
    val effect = SkillEffect.Group(List(damageEff, SkillEffect.ApplyAilment(ailment.ailment, ailment.ailmentRate)))
    Skill(id, effect, element, accuracy, cost, target, strengthBased, personaSkill, criticalRate, buffDebuffs, specialFlags, hits)

  def damaging(
    id: String,
    basePower: Int,
    element: Element,
    accuracy: Int,
    cost: SkillCost,
    target: SkillTarget,
    strengthBased: Boolean = false,
    ailment: Option[SkillAilmentInfo] = None,
    personaSkill: Boolean = true,
    criticalRate: Int = 0,
    buffDebuffs: AppliedBuffs = AppliedBuffs.empty,
    specialFlags: SpecialSkillFlags = SpecialSkillFlags.empty,
    hits: Range = 1 to 1
  ): Skill =
    val damageEff = SkillEffect.Damage(basePower)
    val effect =
      ailment match
        case Some(a) => SkillEffect.Group(List(damageEff, SkillEffect.ApplyAilment(a.ailment, a.ailmentRate)))
        case None => damageEff
    Skill(id, effect, element, accuracy, cost, target, strengthBased, personaSkill, criticalRate, buffDebuffs, specialFlags, hits)
}
final case class Skill
( id: String,
  effect: SkillEffect,
  element: Element,
  accuracy: Int,
  cost: SkillCost,
  target: SkillTarget,
  strengthBased: Boolean = false,
  personaSkill: Boolean = true,
  criticalRate: Int = 0,
  buffDebuffs: AppliedBuffs = AppliedBuffs.empty,
  specialFlags: SpecialSkillFlags = SpecialSkillFlags.empty,
  hits: Range = 1 to 1
) {
  def name: String = L10n.instance.named(id)

  override def toString: String = s"$name ${element.id} effect $effect target $target"
  def canAffordSkill(caster: Fighter, spendsHP: Boolean = true): Boolean = {
    cost.skillCost(caster) match {
      case SkillCost.RealSkillCost.HP(by) => caster.health > by
      // you can drain your SP completely
      case SkillCost.RealSkillCost.SP(by) => caster.sp >= by
    }
  }

  def calcAccuracy(caster: Fighter, target: Fighter)(using rng: PRNG): Boolean =
    if (accuracy >= 255)
      true
    else {
      val accuracyRoot = (caster.stats.agility * 3 + caster.stats.luck + 20).toDouble / (target.stats.agility * 3 + target.stats.luck + 20).toDouble
      val base =
        if (accuracyRoot < 1) {
          1 - (math.sqrt(1 - accuracyRoot) / 30 + (1 - accuracyRoot) / 12)
        } else {
          1 + math.sqrt(accuracyRoot - 1) / 15 + (accuracyRoot - 1) * 2 / 3
        }
      val accuracyStage =
        caster.agilityStatStage match
          case StatStage.Down => 0.7
          case StatStage.Neutral => 1.0
          case StatStage.Up => 1.3
      val evasionStage =
        target.agilityStatStage match
          case StatStage.Down => 1.3
          case StatStage.Neutral => 1.0
          case StatStage.Up => 0.7
      val raw = base * accuracy.toDouble * accuracyStage * evasionStage
      val scaled =
        if (raw < 99) {
          raw
        } else if (raw >= 99 && raw < 114) {
          (raw - 99) / 30 + 99
        } else if (raw >= 114 && raw < 149) {
          (raw - 114) / 70 + 99.5
        } else 100
      
      val random = rng.random() * 100
      scaled > random
    }

  def powerBonus(value: Int)(using rng: PRNG): Int =
    val a = rng.randomIn(0 to (math.max(0, (value.toDouble / 10 - 1).toInt)))
    val b = rng.randomIn(0 to 3)
    value + a + b


  def applyEffect(caster: Fighter, target: Fighter, effect: SkillEffect)(using rng: PRNG): Unit =
    effect match
      case SkillEffect.Damage(power) =>
        val base = BattleMath.getBase(caster.level, if (strengthBased) caster.stats.strength else caster.stats.magic, target.stats.endurance)
        val attackStage =
          caster.attackStatStage match
            case StatStage.Down => 0.6
            case StatStage.Neutral => 1.0
            case StatStage.Up => 1.4
        val defenseStage =
          target.defenseStatStage match
            case StatStage.Down => 1.4
            case StatStage.Neutral => 1.0
            case StatStage.Up => 0.6
        val potential = caster.skillPotentials(element)
        val resist = target.resistances(element)
        
        val modified = base.toDouble * (power.toDouble / 100) * attackStage * defenseStage * potential.damageMultiplier * resist.resistNumber.getOrElse(0.0)
        val res = powerBonus(modified.toInt)
        target.health -= res
      case SkillEffect.Healing(percent, power) =>
        val root = target.maxHP * percent + power
        val base = root * caster.skillPotentials(Element.Healing).healMultiplier.get
        val res = powerBonus(base.toInt)
        target.health += res
      case SkillEffect.ReduceHPTo1 =>
        target.health = 1
      case SkillEffect.ExactDamage(by) =>
        target.health -= by
      case SkillEffect.ExactHealing(by) =>
        target.health += by
      case SkillEffect.HealAllHP =>
        target.health = target.maxHP
      case SkillEffect.RevivePercent(percent) =>
        target.health = (target.maxHP * (percent.toDouble / 100)).toInt
      case SkillEffect.DamagePercentRemaining(percent) => ???
      case SkillEffect.DamagePercentFull(percent) => ???
      case SkillEffect.ApplyAilment(ailment, rate) =>
        lazy val success = {
          val ratio = caster.stats.luck.toDouble / target.stats.luck
          val root =
            if (ratio < 0.5) {
              0.25 - math.sqrt(0.5 - ratio) / 20 + ratio / 2
            } else if (ratio > 1.5) {
              math.sqrt(ratio) + 0.275
            } else ratio
          val raw = root * (rate.toDouble / 100) * caster.skillPotentials.get(element).get.ailmentMultiplier

          val arate = math.min(raw, 100.0).toInt
          arate > rng.randomIn(0 to 100)
        }
        if (rate >= 255 || success) {
          // TODO: handle application
          println("would've applied")
        }
        

      case SkillEffect.CureStatus =>
        println("would've cured")
      case SkillEffect.Group(ls) =>
        ls.foreach(applyEffect(caster, target, _))
      case SkillEffect.None => ()
    

  def performOne(caster: Fighter, target: Fighter)(using rng: PRNG): SkillResult =
    if (!calcAccuracy(caster, target))
      println("missed!")
      return SkillResult.Missed
    
    // Apply effects now we know we hit
    applyEffect(caster, target, effect)


    SkillResult.Unknown

}