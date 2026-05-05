package gay.menkissing
package battle

import SkillCost.RealSkillCost

import scala.math.Ordering.Double.IeeeOrdering
import scala.util.boundary
import common.math as gaymath
import gay.menkissing.engine.L10n

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
case class SkillResult(wasKnockedDown: Boolean = false)

enum SkillEffect {
  case Damage(power: Int)
  case Healing(power: Int)
  case ReduceHPTo1
  case ExactDamage(by: Int)
  case ExactHealing(by: Int)
  case HealAllHP
}

object AppliedBuffs {
  opaque type AppliedBuffs = Int

  extension (self: AppliedBuffs) {
    def attackUp: Boolean = (self & 0x0001) != 0
    def withAttackUp(b: Boolean): AppliedBuffs = (self & ~0x0001) | (if (b) 1 else 0)

    def attackDown: Boolean = (self & 0x0002) != 0
    def withAttackDown(b: Boolean): AppliedBuffs = (self & ~0x0002) | ((if (b) 1 else 0) << 1)

    def agilityUp: Boolean = (self & 0x0004) != 0
    def withAgilityUp(b: Boolean): AppliedBuffs = (self & ~0x0004) | ((if (b) 1 else 0) << 2)

    def agilityDown: Boolean = (self & 0x0008) != 0
    def withAgilityDown(b: Boolean): AppliedBuffs = (self & ~0x0008) | ((if (b) 1 else 0) << 3)

    def defenseUp: Boolean = (self & 0x0010) != 0
    def withDefenseUp(b: Boolean): AppliedBuffs = (self & ~0x0010) | ((if (b) 1 else 0) << 4)

    def defenseDown: Boolean = (self & 0x0020) != 0
    def withDefenseDown(b: Boolean): AppliedBuffs = (self & ~0x0020) | ((if (b) 1 else 0) << 5)

    def powerCharge: Boolean = (self & 0x0040) != 0
    def withPowerCharge(b: Boolean): AppliedBuffs = (self & ~0x0040) | ((if (b) 1 else 0) << 6)

    def mindCharge: Boolean = (self & 0x0080) != 0
    def withMindCharge(b: Boolean): AppliedBuffs = (self & ~0x0080) | ((if (b) 1 else 0) << 7)

    def dekaja: Boolean = (self & 0x0100) != 0
    def withDekaja(b: Boolean): AppliedBuffs = (self & ~0x0100) | ((if (b) 1 else 0) << 8)

    def dekunda: Boolean = (self & 0x0200) != 0
    def withDekunda(b: Boolean): AppliedBuffs = (self & ~0x0200) | ((if (b) 1 else 0) << 9)

    def rebellion: Boolean = (self & 0x0400) != 0
    def withRebellion(b: Boolean): AppliedBuffs = (self & ~0x0400) | ((if (b) 1 else 0) << 10)

    def revolution: Boolean = (self & 0x0800) != 0
    def withRevolution(b: Boolean): AppliedBuffs = (self & ~0x0800) | ((if (b) 1 else 0) << 11)
  }
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
final case class Skill
( id: String,
  basePower: Int,
  element: Element,
  accuracy: Int,
  cost: SkillCost,
  target: SkillTarget,
  strengthBased: Boolean = false,
  ailment: Option[SkillAilmentInfo] = None,
  personaSkill: Boolean = true,
  criticalRate: Int = 0,
  buffDebuffs: BuffDebuffs = BuffDebuffs(),
  hits: Range = 1 to 1
) {
  def name: String = L10n.instance.named(id)

  override def toString: String = s"$name ${element.name} root $basePower target $target"
  def displayStr(caster: Fighter): String = s"$name ${element.name} \u221A $basePower target $target cost ${cost.skillCost(caster)}"
  def validTarget(battle: Battle, target: Fighter): Boolean = {
    target.health > 0
  }
  def canAffordSkill(caster: Fighter, spendsHP: Boolean = true): Boolean = {
    cost.skillCost(caster) match {
      case SkillCost.RealSkillCost.HP(by) => caster.health > by
      // you can drain your SP completely
      case SkillCost.RealSkillCost.SP(by) => caster.sp >= by
    }
  }
  def processSkillCost(caster: Fighter, canSpendHP: Boolean = true): Unit = {
    cost.skillCost(caster) match
      case RealSkillCost.HP(by) => {
        if (canSpendHP) {
          assert(by < caster.health)
          caster.health -= by
        }
      }
      case RealSkillCost.SP(by) => {
        assert(by <= caster.sp)
        caster.sp -= by
      }
  }
  def runIntro(caster: Fighter): Unit = {
    println(s"${caster.name} cast $name!")
  }
  def runAll(battle: Battle, caster: SidedFighter, targets: List[SidedFighter]): List[SkillResult] = {
    assert(target.multiTarget)
    runIntro(caster.fighter)
    targets.filter(it => this.validTarget(battle, it.fighter)).map(it => runCore(battle, caster, it))
  }
  def run(battle: Battle, caster: SidedFighter, target: SidedFighter): SkillResult = {
    assert(!this.target.multiTarget)
    runIntro(caster.fighter)
    runCore(battle, caster, target)
  }
  def calcAccuracy(battle: Battle, caster: Fighter, target: Fighter): Boolean = {
    if (this.accuracy >= 100)
      return true
    val skillHit = this.accuracy.toDouble
    val accuracy = skillHit * (caster.stats.agility.toDouble + 200.0) / (target.stats.agility.toDouble + 200.0)
    // TODO: apply buff
    // TODO: apply passive skills
    val goodAccuracy = gaymath.clamp(accuracy, 50.0, 99.0).toInt
    val randomVal = (math.random() * 100).toInt
    randomVal < goodAccuracy
  }
  def applyAilment(battle:Battle, daCaster: SidedFighter, daTarget: SidedFighter): Option[SkillResult] = {
    val target = daTarget.fighter
    val caster = daCaster.fighter
    val startedKnockdown = caster.isDown
    ailment match {
      case Some(SkillAilmentInfo(ailment, ailmentRate)) => {
        val resist = target.ailmentResistances(ailment)
        if (target.ailmentResistances(ailment) != AilmentResist.NullAilment) {
          // freaky...
          val isGuaranteed = ailment match {
            case Ailment.LightDeath => target.resistances.light == ResistLevel.Weak
            case Ailment.DarkDeath => target.resistances.dark == ResistLevel.Weak
            case _ => false
          }

          val applyRaw = resist.multiplier * (ailmentRate.toDouble) * (caster.stats.luck + 100).toDouble / (target.stats
                                                                                                                  .luck + 100)
            .toDouble
          println(applyRaw)
          val applies = isGuaranteed || (battle.randomIn(0 to 100) < applyRaw.toInt)
          if (applies) {
            ailment.processApplication(battle, daTarget)
            if (target.health <= 0) {
              return Some(SkillResult(wasKnockedDown = isGuaranteed && !startedKnockdown))
            }
          }

        }
      }
      case None => ()
    }
    None
  }
  def runCore(battle: Battle, daCaster: SidedFighter, daTarget: SidedFighter): SkillResult = {
    val caster = daCaster.fighter
    val target = daTarget.fighter
    if (element.damage) {
      if (!target.resistances.get(element).exists(_.stopsDodge)) {
        val didHit = calcAccuracy(battle, caster, target)
        if (!didHit) {
          println(s"${target.name} dodged the attack!")
          return SkillResult()
        }
      }
      val startedKnockdown = target.isDown
      val offenseStat = if (strengthBased) caster.stats.strength else caster.stats.magic
      val affinity = target.resistances.get(element).getOrElse(ResistLevel.Normal)
      val resistLevelMultiplier = affinity match {
        case ResistLevel.Weak => {
          target.isDown = true
          1.5
        }
        case ResistLevel.Normal => {
          1.0
        }
        case ResistLevel.Resist => {
          0.5
        }
        case ResistLevel.NullDamage => {
          println(s"${target.name} nullified the attack!")
          return SkillResult()
        }
        case ResistLevel.Drain => {
          println(s"${target.name} drained the attack for ${basePower}!")
          target.health += basePower
          target.verifyHealth()
          println(s"${target.name} is now at ${target.health}/${target.maxHP}")
          return SkillResult()
        }
        case ResistLevel.Reflect => {
          // TODO: reflect
          println(s"${target.name} would have reflected, but i stupid")
          1.0
        }
      }

      applyAilment(battle, daCaster, daTarget) match {
        case Some(res) => return res
        case _ => ()
      }
      val technicalApplies = target.curAilment.exists(_.technicals.contains(element))
      val resistCategoryMultiplier = if (technicalApplies) 1.5 else resistLevelMultiplier
      val power = math.sqrt(basePower * 15 *  offenseStat / target.stats.endurance) * resistCategoryMultiplier


      val finalPower = (power * (math.random() * 0.1 + 0.95)).toInt
      if (finalPower > 0) {
        println(s"${caster.name} damaged ${target.name} for ${finalPower} damage!")
        if (technicalApplies) {
          println("Technical!")
          target.recoverAilment()
          if (!startedKnockdown) {
            println(s"${target.name} was knocked down!")
          }
          target.isDown = true
        }
        affinity match {
          case ResistLevel.Weak => {
            println(s"${target.name} was weak!")
            if (!startedKnockdown) {
              println(s"${target.name} was knocked down!")
            }
          }
          case ResistLevel.Resist => println(s"${target.name} resisted the damage.")
          case _ => ()
        }
        target.health = target.health - finalPower
        val isValid = target.verifyHealth()
        println(s"${target.name} is now at ${target.health}/${target.maxHP}")
        if (!isValid) {
          battle.processKill(daTarget)
        }
        return SkillResult(wasKnockedDown = !startedKnockdown && target.isDown)
      }
      SkillResult()
    } else {
      element match {
        case Element.Healing => {
          val magicBonus = (caster.stats.magic / 5) * 6
          val baseHealingPower = (basePower.toDouble + magicBonus.toDouble)
          val finalPower = (baseHealingPower * (math.random() * 0.1 + 0.95)).toInt
          target.heal(battle, finalPower)
          return SkillResult()
        }
        case Element.Ailment => {
          applyAilment(battle, daCaster, daTarget)
        }
        case _ => ()
      }
    }
    SkillResult()
  }
}