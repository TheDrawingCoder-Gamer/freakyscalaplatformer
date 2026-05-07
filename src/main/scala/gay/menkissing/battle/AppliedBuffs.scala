package gay.menkissing.battle

import SkillCost.RealSkillCost
import scala.math.Ordering.Double.IeeeOrdering
import scala.util.boundary
import gay.menkissing.common.math as gaymath



opaque type AppliedBuffs = Int
object AppliedBuffs {

  val empty: AppliedBuffs = 0

  def apply(
    attackUp: Boolean = false,
    attackDown: Boolean = false,
    agilityUp: Boolean = false,
    agilityDown: Boolean = false,
    defenseUp: Boolean = false,
    defenseDown: Boolean = false,
    powerCharge: Boolean = false,
    mindCharge: Boolean = false,
    dekaja: Boolean = false,
    dekunda: Boolean = false,
    rebellion: Boolean = false,
    revolution: Boolean = false,
    criticalAura: Boolean = false,
    donumGladi: Boolean = false,
    donumMagici: Boolean = false,
  ): AppliedBuffs =
    empty
      .withAttackUp(attackUp)
      .withAttackDown(attackDown)
      .withAgilityUp(agilityUp)
      .withAgilityDown(agilityDown)
      .withDefenseUp(defenseUp)
      .withDefenseDown(defenseDown)
      .withPowerCharge(powerCharge)
      .withMindCharge(mindCharge)
      .withDekaja(dekaja)
      .withDekunda(dekunda)
      .withRebellion(rebellion)
      .withRevolution(revolution)
      .withCriticalAura(criticalAura)
      .withDonumGladi(donumGladi)
      .withDonumMagici(donumMagici)

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

    def criticalAura: Boolean = (self & 0x1000) != 0
    def withCriticalAura(b: Boolean): AppliedBuffs = (self & ~0x1000) | ((if (b) 1 else 0) << 12)

    def donumGladi: Boolean = (self & 0x2000) != 0
    def withDonumGladi(b: Boolean): AppliedBuffs = (self & ~0x2000) | ((if (b) 1 else 0) << 13)

    def donumMagici: Boolean = (self & 0x4000) != 0
    def withDonumMagici(b: Boolean): AppliedBuffs = (self & ~0x4000) | ((if (b) 1 else 0) << 14)
  }
}