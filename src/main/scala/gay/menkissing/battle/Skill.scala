package gay.menkissing
package battle

import SkillCost.RealSkillCost
import scala.math.Ordering.Double.IeeeOrdering
import scala.util.boundary
import common.math as gaymath

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
( name: String,
  basePower: Int,
  element: Element,
  accuracy: Int,
  cost: SkillCost,
  target: SkillTarget,
  ailment: Option[SkillAilmentInfo] = None,
  personaSkill: Boolean = true,
  criticalRate: Int = 0
) {
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
      val offenseStat = if (element.physical) caster.stats.strength else caster.stats.magic
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

object Skills {
  val bash = Skill("Bash", 50, Element.Strike, 90, SkillCost.HP(7), SkillTarget.Foe)

  val vacuumSlash = Skill("Vacuum Slash", 85, Element.Slash, accuracy = 90, SkillCost.HP(14), SkillTarget.AllFoes)

  private trait MagicPowers {
    val weak: Int
    val medium: Int
    val heavy: Int
    val severeSingle: Int
    val severeMulti: Int
  }
  private object magicPower extends MagicPowers {
    val weak: Int = 40
    val medium: Int = 100
    val heavy: Int = 220
    val severeSingle = 600
    val severeMulti = 450
  }
  private object strongMagicPower extends MagicPowers {
    val weak: Int = 60
    val medium: Int = 140
    val heavy: Int = 350
    val severeSingle = 650
    val severeMulti = 500
  }
  private object magicCost {
    trait SkillCosts {
      val weak: SkillCost
      val medium: SkillCost
      val heavy: SkillCost
      val severe: SkillCost
    }
    trait SkillCostGrouping {
      val single: SkillCosts
      val multi: SkillCosts
    }
    object normal extends SkillCostGrouping {
      object single extends SkillCosts {
        val weak = SkillCost.SP(4)
        val medium = SkillCost.SP(8)
        val heavy = SkillCost.SP(14)
        val severe = SkillCost.SP(46)
      }

      object multi extends SkillCosts {
        val weak = SkillCost.SP(10)
        val medium = SkillCost.SP(16)
        val heavy = SkillCost.SP(24)
        val severe = SkillCost.SP(48)
      }
    }

    object cheap extends SkillCostGrouping {
      object single extends SkillCosts {
        val weak = SkillCost.SP(3)
        val medium = SkillCost.SP(6)
        val heavy = SkillCost.SP(12)
        val severe = SkillCost.SP(40)
      }
      object multi extends SkillCosts {
        val weak = SkillCost.SP(8)
        val medium = SkillCost.SP(14)
        val heavy = SkillCost.SP(24)
        val severe = SkillCost.SP(44)
      }
    }
    object expensive extends SkillCostGrouping {
      object single extends SkillCosts {
        val weak = SkillCost.SP(4)
        val medium = SkillCost.SP(8)
        val heavy = SkillCost.SP(15)
        val severe = SkillCost.SP(48)
      }
      object multi extends SkillCosts {
        val weak = SkillCost.SP(10)
        val medium = SkillCost.SP(16)
        val heavy = SkillCost.SP(25)
        val severe = SkillCost.SP(50)
      }
    }
  }
  private object instakillCost {
    object single {
      val weak = SkillCost.SP(6)
      val strong = SkillCost.SP(14)
    }
    object multi {
      val weak = SkillCost.SP(12)
      val strong = SkillCost.SP(26)
    }
  }
  private object magicAccuracy {
    val single = 99
    val singleSevere = 97
    val multi = 95
  }
  private object magicAilmentRate {
    val single = 15
    val singleSevere = 60
    val multi = 10
    val multiSevere = 35
  }
  private enum MagicStrength {
    case Weak, Medium, Heavy, Severe
  }
  private enum MagicCost {
    case Cheap, Normal, Expensive
  }
  private def ailmentMagic(name: String, strength: MagicStrength, ailment: Ailment, element: Element, multi: Boolean): Skill = {
    val power = strength match {
      case MagicStrength.Weak => magicPower.weak
      case MagicStrength.Medium => magicPower.medium
      case MagicStrength.Heavy => magicPower.heavy
      case MagicStrength.Severe => if (multi) magicPower.severeMulti else magicPower.severeSingle
    }
    val cost = {
      val costGroup = if (multi) magicCost.normal.multi else magicCost.normal.single
      strength match {
        case MagicStrength.Weak => costGroup.weak
        case MagicStrength.Medium => costGroup.medium
        case MagicStrength.Heavy => costGroup.heavy
        case MagicStrength.Severe => costGroup.severe
      }
    }
    val accuracy = if (multi) magicAccuracy.multi else if (strength == MagicStrength.Severe) magicAccuracy.singleSevere else magicAccuracy.single
    val ailmentRate =
      if (multi) {
        if (strength == MagicStrength.Severe)
          magicAilmentRate.multiSevere
        else
          magicAilmentRate.multi
      } else {
        if (strength == MagicStrength.Severe) magicAilmentRate.singleSevere else magicAilmentRate.single
      }
    Skill(name, power, element, accuracy, cost, if (multi) SkillTarget.AllFoes else SkillTarget.Foe, Some(SkillAilmentInfo(ailment, ailmentRate)))
  }
  private def normalMagic(name: String, strength: MagicStrength, element: Element, multi:Boolean, cost: MagicCost = MagicCost.Normal, isStronger: Boolean = false): Skill = {
    val strengthGroup = if (isStronger) strongMagicPower else magicPower
    val power = strength match {
      case MagicStrength.Weak => strengthGroup.weak
      case MagicStrength.Medium => strengthGroup.medium
      case MagicStrength.Heavy => strengthGroup.heavy
      case MagicStrength.Severe => if (multi) strengthGroup.severeMulti else strengthGroup.severeSingle
    }
    val daCost = {
      val costGroupGroup = cost match {
        case MagicCost.Cheap => magicCost.cheap
        case MagicCost.Normal => magicCost.normal
        case MagicCost.Expensive => magicCost.expensive
      }
      val costGroup = if (multi) costGroupGroup.multi else costGroupGroup.single
      strength match {
        case MagicStrength.Weak => costGroup.weak
        case MagicStrength.Medium => costGroup.medium
        case MagicStrength.Heavy => costGroup.heavy
        case MagicStrength.Severe => costGroup.severe
      }
    }
    val accuracy = if (multi) magicAccuracy.multi else if (strength == MagicStrength.Severe) magicAccuracy
      .singleSevere else magicAccuracy.single
    Skill(name, power, element, accuracy, daCost, if (multi) SkillTarget.AllFoes else SkillTarget.Foe, None)
  }
  val agi = ailmentMagic("Agi", MagicStrength.Weak, Ailment.Burn, Element.Fire, false)
  val agilao = ailmentMagic("Agilao", MagicStrength.Medium, Ailment.Burn, Element.Fire, false)
  val agidyne = ailmentMagic("Agidyne", MagicStrength.Heavy, Ailment.Burn, Element.Fire, false)
  val inferno = ailmentMagic("Inferno", MagicStrength.Severe, Ailment.Burn, Element.Fire, false)
  val maragi = ailmentMagic("Maragi", MagicStrength.Weak, Ailment.Burn, Element.Fire, true)
  val maragilao = ailmentMagic("Maragilao", MagicStrength.Medium, Ailment.Burn, Element.Fire, true)
  val maragidyne = ailmentMagic("Maragidyne", MagicStrength.Heavy, Ailment.Burn, Element.Fire, true)
  val blazingHell = ailmentMagic("Blazing Hell", MagicStrength.Severe, Ailment.Burn, Element.Fire, true)

  val bufu = ailmentMagic("Bufu", MagicStrength.Weak, Ailment.Freeze, Element.Ice, false)
  val bufula = ailmentMagic("Bufula", MagicStrength.Medium, Ailment.Freeze, Element.Ice, false)
  val bufudyne = ailmentMagic("Bufudyne", MagicStrength.Heavy, Ailment.Freeze, Element.Ice, false)
  val diamondDust = ailmentMagic("Diamond Dust", MagicStrength.Severe, Ailment.Freeze, Element.Ice, false)
  val mabufu = ailmentMagic("Mabufu", MagicStrength.Weak, Ailment.Freeze, Element.Ice, true)
  val mabufula = ailmentMagic("Mabufula", MagicStrength.Medium, Ailment.Freeze, Element.Ice, true)
  val mabufudyne = ailmentMagic("Mabufudyne", MagicStrength.Heavy, Ailment.Freeze, Element.Ice, true)
  val iceAge = ailmentMagic("Ice Age", MagicStrength.Severe, Ailment.Freeze, Element.Ice, true)

  val zio = ailmentMagic("Zio", MagicStrength.Weak, Ailment.Shock, Element.Electric, false)
  val zionga = ailmentMagic("Zionga", MagicStrength.Medium, Ailment.Shock, Element.Electric, false)
  val ziodyne = ailmentMagic("Ziodyne", MagicStrength.Heavy, Ailment.Shock, Element.Electric, false)
  val thunderReign = ailmentMagic("Thunder Reign", MagicStrength.Severe, Ailment.Shock, Element.Electric, false)
  val mazio = ailmentMagic("Mazio", MagicStrength.Weak, Ailment.Shock, Element.Electric, true)
  val mazionga = ailmentMagic("Mazionga", MagicStrength.Medium, Ailment.Shock, Element.Electric, true)
  val maziodyne = ailmentMagic("Maziodyne", MagicStrength.Heavy, Ailment.Shock, Element.Electric, true)
  val wildThunder = ailmentMagic("Wild Thunder", MagicStrength.Severe, Ailment.Shock, Element.Electric, true)

  val garu = normalMagic("Garu", MagicStrength.Weak, Element.Wind, false, MagicCost.Cheap)
  val garula = normalMagic("Garula", MagicStrength.Medium, Element.Wind, false, MagicCost.Cheap)
  val garudyne = normalMagic("Garudyne", MagicStrength.Heavy, Element.Wind, false, MagicCost.Cheap)
  val pantaRhei = normalMagic("Panta Rhei", MagicStrength.Severe, Element.Wind, false, MagicCost.Cheap)
  val magaru = normalMagic("Magaru", MagicStrength.Weak, Element.Wind, true, MagicCost.Cheap)
  val magarula = normalMagic("Magarula", MagicStrength.Medium, Element.Wind, true, MagicCost.Cheap)
  val magarudyne = normalMagic("Magarudyne", MagicStrength.Heavy, Element.Wind, true, MagicCost.Cheap)
  val vacuumWave = normalMagic("Vacuum Wave", MagicStrength.Severe, Element.Wind, true, MagicCost.Cheap)


  val psi = normalMagic("Psi", MagicStrength.Weak, Element.Psy, false)
  val psio = normalMagic("Psio", MagicStrength.Medium, Element.Psy, false)
  val psiodyne = normalMagic("Psiodyne", MagicStrength.Heavy, Element.Psy, false)
  val psychoForce = normalMagic("Psycho Force", MagicStrength.Severe, Element.Psy, false)
  val mapsi = normalMagic("Mapsi", MagicStrength.Weak, Element.Psy, true)
  val mapsio = normalMagic("Mapsio", MagicStrength.Medium, Element.Psy, true)
  val mapsiodyne = normalMagic("Mapsiodyne", MagicStrength.Heavy, Element.Psy, true)
  val psychoBlast = normalMagic("Psycho Blast", MagicStrength.Severe, Element.Psy, true)

  val frei = normalMagic("Frei", MagicStrength.Weak, Element.Nuke, false)
  val freila = normalMagic("Freila", MagicStrength.Medium, Element.Nuke, false)
  val freidyne = normalMagic("Freidyne", MagicStrength.Heavy, Element.Nuke, false)
  val atomicFlare = normalMagic("Atomic Flare", MagicStrength.Severe, Element.Nuke, false)
  val mafrei = normalMagic("Mafrei", MagicStrength.Weak, Element.Nuke, true)
  val mafreila = normalMagic("Mafreila", MagicStrength.Medium, Element.Nuke, true)
  val mafreidyne = normalMagic("Mafreidyne", MagicStrength.Heavy, Element.Nuke, true)
  val cosmicFlare = normalMagic("Cosmic Flare", MagicStrength.Severe, Element.Nuke, true)

  val dia = Skill("Dia", 50, Element.Healing, 100, SkillCost.SP(3), SkillTarget.Ally)
  val media = Skill("Media", 50, Element.Healing, 100, SkillCost.SP(7), SkillTarget.AllAllies)

  val diarama = Skill("Diarama", 150, Element.Healing, 100, SkillCost.SP(8), SkillTarget.Ally)
  val mediarama = Skill("Mediarama", 150, Element.Healing, 100, SkillCost.SP(16), SkillTarget.AllAllies)


  def instakillSkill(name: String, isLight: Boolean, multi: Boolean, strongVersion: Boolean): Skill = {
    val rateChance = if (strongVersion) 50 else 30
    val element = if (isLight) Element.Light else Element.Dark
    val ailment = if (isLight) Ailment.LightDeath else Ailment.DarkDeath
    val cost =
      if (strongVersion) {
        if (multi) instakillCost.multi.strong else instakillCost.single.strong
      } else {
        if (multi) instakillCost.multi.weak else instakillCost.single.weak
      }
    Skill(name, 0, element, 100, cost, if (multi) SkillTarget.AllFoes else SkillTarget.Foe, ailment = Some(SkillAilmentInfo(ailment, rateChance)))
  }

  val kouha = normalMagic("Kouha", MagicStrength.Weak, Element.Light, false, MagicCost.Expensive, true)
  val kouga = normalMagic("Kouga", MagicStrength.Medium, Element.Light, false, MagicCost.Expensive, true)
  val kougaon = normalMagic("Kougaon", MagicStrength.Heavy, Element.Light, false, MagicCost.Expensive, true)
  val makouha = normalMagic("Makouha", MagicStrength.Weak, Element.Light, true, MagicCost.Expensive, true)
  val makouga = normalMagic("Makouga", MagicStrength.Medium, Element.Light, true, MagicCost.Expensive, true)
  val makougaon = normalMagic("Makougaon", MagicStrength.Heavy, Element.Light, true, MagicCost.Expensive, true)
  val hama = instakillSkill("Hama", true, false, false)
  val hamaon = instakillSkill("Hamaon", true, false, true)
  val mahama = instakillSkill("Mahama", true, true, false)
  val mahamaon = instakillSkill("Mahamaon", true, true, true)

  val eiha = normalMagic("Eiha", MagicStrength.Weak, Element.Dark, false, MagicCost.Expensive, true)
  val eiga = normalMagic("Eiga", MagicStrength.Medium, Element.Dark, false, MagicCost.Expensive, true)
  val eigaon = normalMagic("Eigaon", MagicStrength.Heavy, Element.Dark, false, MagicCost.Expensive, true)
  val maeiha = normalMagic("Maeiha", MagicStrength.Weak, Element.Dark, true, MagicCost.Expensive, true)
  val maeiga = normalMagic("Maeiga", MagicStrength.Medium, Element.Dark, true, MagicCost.Expensive, true)
  val maeigaon = normalMagic("Maeigaon", MagicStrength.Heavy, Element.Dark, true, MagicCost.Expensive, true)
  val mudo = instakillSkill("Mudo", false, false, false)
  val mudoon = instakillSkill("Mudoon", false, false, true)
  val mamudo = instakillSkill("Mamudo", false, true, false)
  val mamudoon = instakillSkill("Mamudoon", false, true, true)

  private def forceApplyAilment(ailment: Ailment, element: Element = Element.Ailment): Skill =
    Skill(s"DEBUG: Apply $ailment", 0, element, 100, SkillCost.SP(1), SkillTarget.Foe, Some(SkillAilmentInfo(ailment, 100)))
  val forceApplyBurn = forceApplyAilment(Ailment.Burn, Element.Fire)
  val forceApplyFreeze = forceApplyAilment(Ailment.Freeze, Element.Ice)
  val forceApplyShock = forceApplyAilment(Ailment.Shock, Element.Electric)
  val forceApplyDizzy = forceApplyAilment(Ailment.Dizzy)
  val forceApplySleep = forceApplyAilment(Ailment.Sleep)
  val forceApplyForget = forceApplyAilment(Ailment.Forget)
  val forceApplyFear = forceApplyAilment(Ailment.Fear)
  val forceApplyRage = forceApplyAilment(Ailment.Rage)
  val forceApplyBrainwash = forceApplyAilment(Ailment.Brainwash)
}