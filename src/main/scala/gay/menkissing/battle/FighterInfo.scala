package gay.menkissing.battle

// Fixed State that _can't_ change during the course of a battle
// This doesn't mean it can't change between battles.
final case class FighterInfo(
                            level: Int,
                            maxHP: Int,
                            maxSP: Int
                            )
