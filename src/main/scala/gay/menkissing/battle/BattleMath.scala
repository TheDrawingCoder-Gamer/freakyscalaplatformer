package gay.menkissing.battle

object BattleMath {
  def getRoot(level: Int): Int =
    // ??? does this matter
    if (level > 150) {
      (level / 10) + 145
    } else {
      level + 10
    }
  def deriveOffense(root: Int, stat: Int): Int =
    if (stat <= root) {
      root + stat
    } else {
      ((math.sqrt(stat - root) / 2) + stat / 2 + (root * 3 / 2)).toInt
    }

  def getBase(level: Int, stat: Int, defenderEn: Int): Int =
    val root = getRoot(level)
    val offense = deriveOffense(root, stat)
    val diff = offense - defenderEn
    val r =
      if (diff <= offense / 2) {
        ((offense * 2.0 / 3) - defenderEn.toDouble / 3 - math.sqrt(defenderEn - offense / 2) / 3).toInt
      } else if (diff > offense * 3 / 4) {
        ((offense * 5.0 / 6) - defenderEn.toDouble / 3 + math.sqrt(offense / 4.0 - defenderEn) / 3).toInt
      } else {
        offense - defenderEn
      }
    math.max(1, r)
}
