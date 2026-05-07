package gay.menkissing
package battle

import engine.group.*
import engine.*

class EnemySelector extends GayObjectContainer {
  val bar = new PercentBar(128, 20, 2)
  
  add(bar)

  def updateSelection(enemy: Enemy): Unit = {
    val percent = enemy.fighter.health.toDouble / enemy.fighter.maxHP
    bar.percentage = (percent * 100).toInt
    this.x = enemy.x * draw.scaleRatio
    this.y = enemy.y * draw.scaleRatio
  }
}
