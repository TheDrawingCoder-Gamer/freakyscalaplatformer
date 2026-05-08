package gay.menkissing.battle

class HPBar(fullWidth: Int, fullHeight: Int, inPadding: Int, val maximum: Int) extends PercentBar(fullWidth, fullHeight, inPadding) {
  def setValue(value: Int): Unit =
    percentage = value.toDouble / maximum
}
