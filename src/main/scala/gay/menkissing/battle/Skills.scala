package gay.menkissing.battle

object Skills {
  private def phys(rawId: String, cost: SkillCost, multi: Boolean, basePower: Int, critRate: Int, accuracy: Int, hits: Range = 1 to 1, ailment: Option[SkillAilmentInfo] = None, buffDebuffs: AppliedBuffs = AppliedBuffs.empty, specialFlags: SpecialSkillFlags = SpecialSkillFlags.empty)(using element: Element): Skill =
    val target = if (multi) SkillTarget.AllFoes else SkillTarget.Foe
    Skill.damaging(s"skill.${element.id}.$rawId", basePower, element, accuracy, cost, target, strengthBased = true, ailment = ailment, criticalRate = critRate, buffDebuffs = buffDebuffs, specialFlags = specialFlags, hits = hits)

  object strike {
    private given Element = Element.Strike

    val bash = phys("bash", SkillCost.HP(7), false, 120, 0, 90)
    val giganticFist = phys("gigantic_fist", SkillCost.HP(14), false, 190, 20, 85)
    val swiftStrike = phys("swift_strike", SkillCost.HP(15), true, 70, 0, 80, hits = 1 to 2)
    val kidneySmash = phys("kidney_smash", SkillCost.HP(9), false, 70, 20, 92, buffDebuffs = AppliedBuffs(attackDown = true))
    val assaultDive = phys("assault_dive", SkillCost.HP(11), false, 110, 20, 88)
    val armorSplitter = phys("armor_splitter", SkillCost.HP(12), false, 110, 0, 92, buffDebuffs = AppliedBuffs(defenseDown = true))
    val herculeanStrike = phys("herculean_strike", SkillCost.HP(17), true, 150, 0, 85)
    val heatWave = phys("heat_wave", SkillCost.HP(20), true, 200, 0, 90)
    val akashaArts = phys("akasha_arts", SkillCost.HP(22), true, 170, 20, 88, hits = 1 to 2)
    val godsHand = phys("gods_hand", SkillCost.HP(25), false, 300, 20, 88)
  }

  object slash {
    private given Element = Element.Slash

    val powerSlash = phys("power_slash", SkillCost.HP(7), false, 70, 0, 92)
    val tempestSlash = phys("tempest_slash", SkillCost.HP(14), false, 70, 0, 95, hits = 1 to 4)
    val fatalEnd = phys("fatal_end", SkillCost.HP(10), false, 110, 0, 95, ailment = Some(SkillAilmentInfo(Ailment.Fear, 35)))
    val guillotine = phys("guillotine", SkillCost.HP(12), false, 110, 0, 95, specialFlags = SpecialSkillFlags(boostedAgainstAilment = true))
    val legSlice = phys("leg_slice", SkillCost.HP(12), false, 110, 0, 95, buffDebuffs = AppliedBuffs(agilityDown = true))
    val braveBlade = phys("brave_blade", SkillCost.HP(20), false, 250, 20, 90)
    val vacuumSlash = phys("vacuum_slash", SkillCost.HP(14), true, 90, 0, 90)
    val bladeOfFury = phys("blade_of_fury", SkillCost.HP(17), true, 60, 0, 80, hits = 2 to 4)
    val deathbound = phys("deathbound", SkillCost.HP(19), true, 180, 20, 85)
    val vorpalBlade = phys("vorpal_blade", SkillCost.HP(22), true, 240, 0, 92)
    val heavensBlade = phys("heavens_blade", SkillCost.HP(18), false, 220, 30, 90)
    val crossSlash = phys("cross_slash", SkillCost.HP(18), false, 120, 0, 99, hits = 2 to 2)
    val masquerade = phys("masquerade", SkillCost.HP(22), true, 140, 30, 90, hits = 2 to 2)
  }

  object pierce {
    private given Element = Element.Pierce

    val arrowRain = phys("arrow_rain", SkillCost.HP(16), true, 50, 0, 85, hits = 1 to 4)
    val vileAssault = phys("vile_assault", SkillCost.HP(15), false, 200, 0, 95, specialFlags = SpecialSkillFlags(bonusAgainstDowned = true))
    val cruelAttack = phys("cruel_attack", SkillCost.HP(13), false, 140, 0, 95, specialFlags = SpecialSkillFlags(bonusAgainstDowned = true))
    val primalForce = phys("primal_force", SkillCost.HP(21), false, 240, 20, 92)
    val torrentShot = phys("torrent_shot", SkillCost.HP(11), false, 50, 20, 85, hits = 2 to 4)
    val holyArrow = phys("holy_arrow", SkillCost.HP(8), false, 80, 0, 92, ailment = Some(SkillAilmentInfo(Ailment.Forget, 35)))
    val myriadArrows = phys("myriad_arrows", SkillCost.HP(19), true, 60, 0, 88, hits = 2 to 5)
    val oneShotKill = phys("one_shot_kill", SkillCost.HP(17), false, 150, 30, 90)
    val pralaya = phys("pralaya", SkillCost.HP(23), true, 240, 20, 95, ailment = Some(SkillAilmentInfo(Ailment.Fear, 50)))

  }


  private trait MagicPowers {
    val weak: Int
    val medium: Int
    val heavy: Int
    val severe: Int
  }

  private trait SingleMagicPowers extends MagicPowers {
    val bloody: Int
  }

  private trait MagicPowersGroup {
    val single: SingleMagicPowers
    val multi: MagicPowers
  }

  private object magicPowers extends MagicPowersGroup {
    object single extends SingleMagicPowers {
      val weak: Int = 130
      val medium: Int = 160
      val bloody: Int = 200
      val heavy: Int = 215
      val severe: Int = 265
    }

    object multi extends MagicPowers {
      val weak: Int = 95
      val medium: Int = 120
      val heavy: Int = 155
      val severe: Int = 185
    }
  }

  private object strongMagicPowers extends MagicPowersGroup {
    object single extends SingleMagicPowers {
      val weak: Int = 140
      val medium: Int = 175
      val heavy: Int = 265
      val bloody: Int = 220
      val severe: Int = 330
    }

    object multi extends MagicPowers {
      val weak: Int = 105
      val medium: Int = 135
      val heavy: Int = 185
      val severe: Int = 250
    }
  }



  private object magicCost {
    trait SkillCosts {
      val weak: SkillCost
      val medium: SkillCost
      val heavy: SkillCost
      val severe: SkillCost
    }
    trait SingleSkillCosts extends SkillCosts {
      val bloody: SkillCost
    }

    trait SkillCostGrouping {
      val single: SingleSkillCosts
      val multi: SkillCosts
    }

    object normal extends SkillCostGrouping {
      object single extends SingleSkillCosts {
        val weak = SkillCost.SP(10)
        val medium = SkillCost.SP(20)
        val bloody = SkillCost.HP(25)
        val heavy = SkillCost.SP(35)
        val severe = SkillCost.SP(50)
      }

      object multi extends SkillCosts {
        val weak = SkillCost.SP(15)
        val medium = SkillCost.SP(30)
        val heavy = SkillCost.SP(50)
        val severe = SkillCost.SP(75)
      }
    }

    object cheap extends SkillCostGrouping {
      object single extends SingleSkillCosts {
        val weak = SkillCost.SP(8)
        val medium = SkillCost.SP(18)
        val bloody = SkillCost.HP(20)
        val heavy = SkillCost.SP(32)
        val severe = SkillCost.SP(45)
      }

      object multi extends SkillCosts {
        val weak = SkillCost.SP(13)
        val medium = SkillCost.SP(25)
        val heavy = SkillCost.SP(45)
        val severe = SkillCost.SP(70)
      }
    }

    object expensive extends SkillCostGrouping {
      object single extends SingleSkillCosts {
        val weak = SkillCost.SP(15)
        val medium = SkillCost.SP(25)
        val bloody = SkillCost.HP(30)
        val heavy = SkillCost.SP(55)
        val severe = SkillCost.SP(75)
      }

      object multi extends SkillCosts {
        val weak = SkillCost.SP(25)
        val medium = SkillCost.SP(40)
        val heavy = SkillCost.SP(80)
        val severe = SkillCost.SP(140)
      }
    }
  }


  private object magicAccuracy {
    val single = 99
    val bloody = 96
    val singleSevere = 98
    val multi = 95
  }

  private trait AilmentRates {
    val single: Int
    val singleBloody: Int
    val singleSevere: Int
    val multi: Int
    val multiSevere: Int
  }

  private object magicAilmentRate extends AilmentRates {
    val single = 15
    val singleBloody = 30
    val singleSevere = 60
    val multi = 15
    val multiSevere = 35
  }

  // instakills only apply when hitting weakness, so this isnt THAT insane
  // to add in addition to high damage
  private object instakillRates extends AilmentRates {
    val single = 40
    val singleBloody = 45
    val singleSevere = 50
    val multi = 30
    val multiSevere = 40
  }

  // Suprised it lets me do private[Skills] but not complaining :joy:
  class PlainMagic private[Skills] (
                  element: Element,
                  powers: MagicPowersGroup,
                  costs: magicCost.SkillCostGrouping,
                  associatedAilment: Option[Ailment],
                  ailmentRates: AilmentRates = magicAilmentRate
                  ) {
    def getAilment(rate: Int): Option[SkillAilmentInfo] =
      associatedAilment.map { x =>
        SkillAilmentInfo(x, rate)
      }

    val weak = Skill.damaging(s"skill.${element.id}.weak", powers.single.weak, element, magicAccuracy.single, costs.single.weak, SkillTarget.Foe, ailment = getAilment(ailmentRates.single))
    val medium = Skill.damaging(s"skill.${element.id}.medium", powers.single.medium, element, magicAccuracy.single, costs.single.medium, SkillTarget.Foe, ailment = getAilment(ailmentRates.single))
    val heavy = Skill.damaging(s"skill.${element.id}.heavy", powers.single.heavy, element, magicAccuracy.single, costs.single.heavy, SkillTarget.Foe, ailment = getAilment(ailmentRates.single))
    val bloody = Skill.damaging(s"skill.${element.id}.bloody", powers.single.bloody, element, magicAccuracy.bloody, costs.single.bloody, SkillTarget.Foe, ailment = getAilment(ailmentRates.singleBloody))
    val severe = Skill.damaging(s"skill.${element.id}.severe", powers.single.severe, element, magicAccuracy.singleSevere, costs.single.severe, SkillTarget.Foe, ailment = getAilment(ailmentRates.singleSevere))
    val weakMulti = Skill.damaging(s"skill.${element.id}.weak_multi", powers.multi.weak, element, magicAccuracy.multi, costs.multi.weak, SkillTarget.AllFoes, ailment = getAilment(ailmentRates.multi))
    val mediumMulti = Skill.damaging(s"skill.${element.id}.medium_multi", powers.multi.medium, element, magicAccuracy.multi, costs.multi.medium, SkillTarget.AllFoes, ailment = getAilment(ailmentRates.multi))
    val heavyMulti = Skill.damaging(s"skill.${element.id}.heavy_multi", powers.multi.heavy, element, magicAccuracy.multi, costs.multi.heavy, SkillTarget.AllFoes, ailment = getAilment(ailmentRates.multi))
    val severeMulti = Skill.damaging(s"skill.${element.id}.severe_multi", powers.multi.severe, element, magicAccuracy.multi, costs.multi.severe, SkillTarget.AllFoes, ailment = getAilment(ailmentRates.multiSevere))
  }

  class TechnicalMagic private[Skills] (
                  element: Element,
                  powers: MagicPowersGroup,
                  costs: magicCost.SkillCostGrouping,
                  ) extends PlainMagic(element, powers, costs, None) 
                  {
    val technicalBoosted = Skill.damaging(s"skill.${element.id}.technical_boosted", powers.single.medium, element, magicAccuracy.single, SkillCost.SP(25), SkillTarget.Foe)
  }

  val fire = new PlainMagic(Element.Fire, magicPowers, magicCost.normal, Some(Ailment.Burn))
  val ice = new PlainMagic(Element.Ice, magicPowers, magicCost.normal, Some(Ailment.Freeze))
  val electric = new PlainMagic(Element.Electric, magicPowers, magicCost.normal, Some(Ailment.Shock))
  val wind = new PlainMagic(Element.Wind, magicPowers, magicCost.cheap, None)
  val psy = new TechnicalMagic(Element.Psy, magicPowers, magicCost.normal)
  val nuke = new TechnicalMagic(Element.Nuke, magicPowers, magicCost.normal)
  val bless = new PlainMagic(Element.Bless, strongMagicPowers, magicCost.expensive, Some(Ailment.LightDeath), instakillRates)
  val curse = new PlainMagic(Element.Curse, strongMagicPowers, magicCost.expensive, Some(Ailment.DarkDeath), instakillRates)



  object healing {
    val weak = Skill("skill.healing.heal.weak", SkillEffect.Healing(0.15, 35), Element.Healing, Skill.maxAccuracy, SkillCost.SP(8), SkillTarget.Ally)
    val weakMulti = Skill("skill.healing.heal.weak_multi", SkillEffect.Healing(0.13, 30), Element.Healing, Skill.maxAccuracy, SkillCost.SP(30), SkillTarget.AllAllies)

    val medium = Skill("skill.healing.heal.medium", SkillEffect.Healing(0.25, 80), Element.Healing, Skill.maxAccuracy, SkillCost.SP(15), SkillTarget.Ally)
    val mediumMulti = Skill("skill.healing.heal.medium_multi", SkillEffect.Healing(0.22, 65), Element.Healing, Skill.maxAccuracy, SkillCost.SP(60), SkillTarget.AllAllies)

    val full = Skill("skill.healing.heal.full", SkillEffect.HealAllHP, Element.Healing, Skill.maxAccuracy, SkillCost.SP(35), SkillTarget.Ally)
    val fullMulti = Skill("skill.healing.heal.full_multi", SkillEffect.HealAllHP, Element.Healing, Skill.maxAccuracy, SkillCost.SP(150), SkillTarget.AllAllies)

    val cureStatus = Skill("skill.healing.cure_status", SkillEffect.CureStatus, Element.Healing, Skill.maxAccuracy, SkillCost.SP(8), SkillTarget.Ally)
    val cureStatusMulti = Skill("skill.healing.cure_status_multi", SkillEffect.CureStatus, Element.Healing, Skill.maxAccuracy, SkillCost.SP(30), SkillTarget.AllAllies)

    val salvation = Skill("skill.healing.salvation", SkillEffect.Group(List(SkillEffect.HealAllHP, SkillEffect.CureStatus)), Element.Healing, Skill.maxAccuracy, SkillCost.SP(200), SkillTarget.AllAllies)

    //val reviveHalf = Skill("skill.healing.revive_half", 0, Element.Healing, 100, SkillCost.SP(20), SkillTarget.Ally)
  }
  
  object almighty {
    val medium = Skill.damaging("skill.almighty.medium", 125, Element.Almighty, 95, SkillCost.SP(40), SkillTarget.AllFoes)
    val heavy = Skill.damaging("skill.almighty.heavy", 160, Element.Almighty, 95, SkillCost.SP(70), SkillTarget.AllFoes)
    val severe = Skill.damaging("skill.almighty.severe", 190, Element.Almighty, 95, SkillCost.SP(120), SkillTarget.AllFoes)
  }



  

  private final case class TargetingPair(single: Int, multi: Int)

  private object AilmentCosts {
    val single = SkillCost.SP(10)
    val multi = SkillCost.SP(25)
  }

  private val lowAilmentRates = TargetingPair(60, 45)
  private val highAilmentRates = TargetingPair(70, 50)

  class AilmentSkills private[Skills](
                                     ailment: Ailment,
                                     rates: TargetingPair
                                     ) {
    val single: Skill = Skill(
      s"skill.ailment.${ailment.id}",
      SkillEffect.ApplyAilment(ailment, rates.single),
      Element.Ailment,
      Skill.maxAccuracy,
      AilmentCosts.single,
      SkillTarget.Foe
    )
    val multi: Skill = Skill(
      s"skill.ailment.${ailment.id}_multi",
      SkillEffect.ApplyAilment(ailment, rates.multi),
      Element.Ailment,
      Skill.maxAccuracy,
      AilmentCosts.multi,
      SkillTarget.AllFoes,
    )
  }

  val fear = new AilmentSkills(Ailment.Fear, lowAilmentRates)
  val rage = new AilmentSkills(Ailment.Rage, highAilmentRates)
  val confuse = new AilmentSkills(Ailment.Confuse, lowAilmentRates)
  val despair = new AilmentSkills(Ailment.Despair, highAilmentRates)
  val forget = new AilmentSkills(Ailment.Forget, highAilmentRates)

  def supportSkill(id: String, target: SkillTarget, cost: SkillCost, buffDebuffs: AppliedBuffs): Skill =
    Skill(s"skill.support.$id", SkillEffect.None, Element.Support, 100, cost, target, false, buffDebuffs = buffDebuffs)

  object support {
    private val chargeCost = SkillCost.SP(30)
    private val donumCost = SkillCost.SP(40)
    private val costStatUp = SkillCost.SP(8)
    private val costStatDown = SkillCost.SP(8)
    private val costStatUpAll = SkillCost.SP(24)
    private val costStatDownAll = SkillCost.SP(30)
    private val costAllStat = SkillCost.SP(40)
    private val costAllStatAll = SkillCost.SP(150)

    val charge = supportSkill("charge", SkillTarget.Self, chargeCost, AppliedBuffs(powerCharge = true))
    val concentrate = supportSkill("concentrate", SkillTarget.Self, chargeCost, AppliedBuffs(mindCharge = true))
    val bloodyCharge = supportSkill("bloody_charge", SkillTarget.Self, SkillCost.HP(40), AppliedBuffs(powerCharge = true, rebellion = true))
    val debilitate = supportSkill("debilitate", SkillTarget.Foe, costAllStat, AppliedBuffs(attackDown = true, defenseDown = true, agilityDown = true))
    val dekaja = supportSkill("dekaja", SkillTarget.AllFoes, SkillCost.SP(40), AppliedBuffs(dekaja = true))
    val dekunda = supportSkill("dekunda", SkillTarget.AllAllies, SkillCost.SP(40), AppliedBuffs(dekunda = true))
    val heatRiser = supportSkill("heat_riser", SkillTarget.Ally, costAllStat, AppliedBuffs(attackUp = true, defenseUp = true, agilityUp = true))
    val rakukaja = supportSkill("defense_up", SkillTarget.Ally, costStatUp, AppliedBuffs(defenseUp = true))
    val marakukaja = supportSkill("defense_up_all", SkillTarget.AllAllies, costStatUpAll, AppliedBuffs(defenseUp = true))
    val tarukaja = supportSkill("attack_up", SkillTarget.Ally, costStatUp, AppliedBuffs(attackUp = true))
    val matarukaja = supportSkill("attack_up_all", SkillTarget.AllAllies, costStatUpAll, AppliedBuffs(attackUp = true))
    val sukukaja = supportSkill("agility_up", SkillTarget.Ally, costStatUp, AppliedBuffs(agilityUp = true))
    val masukukaja = supportSkill("agility_up_all", SkillTarget.AllAllies, costStatUpAll, AppliedBuffs(agilityUp = true))
    val rakunda = supportSkill("defense_down", SkillTarget.Foe, costStatDown, AppliedBuffs(defenseDown = true))
    val marakunda = supportSkill("defense_down_all", SkillTarget.AllFoes, costStatDownAll, AppliedBuffs(defenseDown = true))
    val tarunda = supportSkill("attack_down", SkillTarget.Foe, costStatDown, AppliedBuffs(attackDown = true))
    val matarunda = supportSkill("attack_down_all", SkillTarget.AllFoes, costStatDownAll, AppliedBuffs(attackDown = true))
    val sukunda = supportSkill("agility_down", SkillTarget.Foe, costStatDown, AppliedBuffs(agilityDown = true))
    val masukunda = supportSkill("agility_down_all", SkillTarget.AllFoes, costStatDownAll, AppliedBuffs(agilityDown = true))
    val criticalAura = supportSkill("critical_aura", SkillTarget.Self, SkillCost.SP(25), AppliedBuffs(criticalAura = true))
    val heatRiserAll = supportSkill("heat_riser_all", SkillTarget.AllAllies, costAllStatAll, AppliedBuffs(attackUp = true, defenseUp = true, agilityUp = true))
    val trumpetsOfDoom = supportSkill("debilitate_all", SkillTarget.AllFoes, costAllStatAll, AppliedBuffs(attackDown = true, defenseDown = true, agilityDown = true))
    val donumGladi = supportSkill("donum_gladi", SkillTarget.Ally, donumCost, AppliedBuffs(donumGladi = true))
    val donumMagici = supportSkill("donum_magici", SkillTarget.Ally, donumCost, AppliedBuffs(donumMagici = true))

  }


  private def forceApplyAilment(ailment: Ailment, element: Element = Element.Ailment): Skill =
    Skill(s"skill.debug.${ailment.id}", SkillEffect.ApplyAilment(ailment, 100), element, Skill.maxAccuracy, SkillCost.SP(1), SkillTarget
      .Foe, false)

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
