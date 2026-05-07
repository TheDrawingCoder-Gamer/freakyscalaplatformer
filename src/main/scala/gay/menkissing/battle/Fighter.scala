package gay.menkissing.battle

trait Fighter {
  def name: String

  def maxHP: Int
  var health: Int

  def maxSP: Int
  var sp: Int

  def level: Int

  def infoStr: String = s"${name} HP: ${health}/$maxHP SP: $sp/$maxSP ${if (isDown) "Downed" else ""}"


  def skills: List[Skill]

  def stats: Stats

  def resistances: ResistElementMap
  def ailmentResistances: ResistAilmentMap
  def skillPotentials: PotentialElementMap

  var isDown: Boolean

  var curAilment: Option[Ailment] = None
  var ailmentTimer: Int = 0

  var attackStatStage: StatStage = StatStage.Neutral
  var defenseStatStage: StatStage = StatStage.Neutral
  var agilityStatStage: StatStage = StatStage.Neutral

  var hasCharge: Boolean = false
  var hasConcentrate: Boolean = false
  var hasDonumGladi: Boolean = false
  var hasDonumMagici: Boolean = false

  // Multiplier used when this fighter is the attacker
  def accuracyMultiplier: Double = {
    var multiplier: Double = 1.0
    curAilment.foreach {
      case Ailment.Dizzy => multiplier *= 0.3
      case Ailment.Rage => multiplier *= 0.5
      case _ => ()
    }
    multiplier
  }
  // Multiplier used when this fighter is the defender
  def evasionMultiplier: Double = {
    1.0
  }
  def attackMultiplier: Double = {
    if (curAilment.contains(Ailment.Rage)) 2.0 else 1.0
  }
  // damage taken when being attacked
  def defenseMultiplier: Double = {
    var multiplier = if (curAilment.contains(Ailment.Rage)) 2.0 else 1.0
    if (isDown) multiplier *= 1.25
    multiplier
  }
  def verifyHealth(): Boolean = {
    if (health < 0) health = 0
    if (health > maxHP) health = maxHP
    health > 0
  }
  def verifySP(): Unit = {
    if (sp < 0) sp = 0
    if (sp > maxSP) sp = maxSP
  }
  def recoverAilment(): Unit = {
    curAilment.foreach { ailment =>
      curAilment = None
      println(s"$name recovered from $ailment")
    }
  }

  def turnTimers(): Unit = {
    curAilment.foreach { ailment =>
      ailmentTimer -= 1
      if (ailmentTimer <= 0) {
        curAilment = None
        println(s"$name's $ailment was healed")
      }
    }
  }

  def basicAttackSkill: Skill
}

class BasicFighter(val name: String,
                   val maxHP: Int,
                   val maxSP: Int,
                   val level: Int,
                   val stats: Stats,
                   val skills: List[Skill],
                   val basicAffinity: Element = Element.Slash,
                   val basicPower: Int,
                   val resistances: ResistElementMap,
                   val ailmentResistances: ResistAilmentMap = ResistAilmentMap(),
                   val skillPotentials: PotentialElementMap = PotentialElementMap()) extends Fighter {
  var health: Int = maxHP
  var sp: Int = maxSP
  var isDown: Boolean = false

  val basicAttackSkill: Skill = Skill("Basic Attack", SkillEffect.Damage(basicPower / 2), basicAffinity, 90, SkillCost.HP(0), SkillTarget.Foe, personaSkill = false)

}


