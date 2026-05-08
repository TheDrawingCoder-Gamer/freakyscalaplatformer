package gay.menkissing
package battle

import gay.menkissing.engine.GaySprite
import engine.group.*
import gay.menkissing.engine.GayTexture

class Enemy(val fighter: Fighter, texture: draw.Texture) extends GayObjectContainer {
  val spr = GaySprite(GayTexture(texture))
  spr.scaleX = draw.scaleRatio
  spr.scaleY = draw.scaleRatio
  add(spr)
  val selector = new EnemySelector()
  add(selector)
  selector.visible = false

  fighter.healthUpdateSignal.add { hp =>
    val ratio = hp.toDouble / fighter.maxHP

    selector.bar.percentage = ratio
  }
}
