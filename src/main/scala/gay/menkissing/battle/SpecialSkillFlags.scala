package gay.menkissing.battle

opaque type SpecialSkillFlags = Int

object SpecialSkillFlags {
  val empty: SpecialSkillFlags = 0

  def apply(
    boostedTechnical: Boolean = false,
    boostedAgainstAilment: Boolean = false,
    bonusAgainstDowned: Boolean = false,
  ): SpecialSkillFlags =
    empty
      .withBoostedTechnical(boostedTechnical)
      .withBoostedAgainstAilment(boostedAgainstAilment)
      .withBonusAgainstDowned(bonusAgainstDowned)

  extension (x: SpecialSkillFlags) {
    def boostedTechnical: Boolean = (x & 0x0001) != 0
    def withBoostedTechnical(b: Boolean): SpecialSkillFlags =
      (x & ~0x0001) | (if (b) 1 << 0 else 0)
    
    def boostedAgainstAilment: Boolean = (x & 0x0002) != 0
    def withBoostedAgainstAilment(b: Boolean): SpecialSkillFlags =
      (x & ~0x0002) | (if (b) 1 << 1 else 0)
    
    def bonusAgainstDowned: Boolean = (x & 0x0004) != 0
    def withBonusAgainstDowned(b: Boolean): SpecialSkillFlags =
      (x & ~0x0004) | (if (b) 1 << 2 else 0)

  }
}
