package gay.menkissing
package battle

import engine.group.*
import engine.*

class EnemySelector extends GayObjectContainer {
  val bar = new DamageBar(128, 20, 2)
  
  add(bar)

}
