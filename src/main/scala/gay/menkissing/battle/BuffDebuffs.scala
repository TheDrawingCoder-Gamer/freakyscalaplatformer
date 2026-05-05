package gay.menkissing.battle

final case class BuffDebuffs(
                            attackUp: Boolean = false,
                            attackDown: Boolean = false,
                            agilityUp: Boolean = false,
                            agilityDown: Boolean = false,
                            defenseUp: Boolean = false,
                            defenseDown: Boolean = false,
                            charge: Boolean = false,
                            concentrate: Boolean = false,
                            donumGladi: Boolean = false,
                            donumMagici: Boolean = false,
                            dekaja: Boolean = false,
                            dekunda: Boolean = false,
                            rebellion: Boolean = false,
                            revolution: Boolean = false
                            )

object BuffDebuffs {

}
