package gay.menkissing
package battle

import gay.menkissing.engine.GaySprite
import engine.group.*
import gay.menkissing.engine.GayTexture
import gay.menkissing.engine.graphics.Texture
import gay.menkissing.engine.anim.GayTween
import gay.menkissing.engine.anim.TweenAccessor
import gay.menkissing.GraphicG

class Enemy(val fighter: Fighter, texture: Texture) extends GayObjectContainer {
  val spr = GaySprite(GayTexture(texture))
  spr.scaleX = GraphicG.scaleRatio
  spr.scaleY = GraphicG.scaleRatio
  add(spr)
  val selector = new EnemySelector()
  add(selector)
  selector.visible = false

  fighter.healthUpdateSignal.add { hp =>
    val ratio = hp.toDouble / fighter.maxHP

    selector.bar.percentage = ratio

  }
}
