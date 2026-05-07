package gay.menkissing
package battle

import engine.group.*
import gay.menkissing.engine.GaySprite
import gay.menkissing.engine.GayTexture

class BattlePortrait(val fighter: Fighter, portrait: draw.Texture) extends GayObjectContainer {
  val portraitSprite = GaySprite(GayTexture(portrait))
  portraitSprite.scaleX = 2.0
  portraitSprite.scaleY = 2.0
  add(portraitSprite)
  val hpBar = HPBar(portraitSprite.graphicalWidth, BattlePortrait.barHeight, BattlePortrait.barPadding, fighter.maxHP)
  add(hpBar)
  hpBar.y += portraitSprite.graphicalHeight
  val spBar = HPBar(portraitSprite.graphicalWidth, BattlePortrait.barHeight, BattlePortrait.barPadding, fighter.maxSP)
  add(spBar)
  spBar.healthRect.rect.color = draw.Color.fromHex(0xFFFF00FF)
  spBar.y = hpBar.y + BattlePortrait.barHeight
}

object BattlePortrait {
  val barHeight: Int = 20
  val barPadding: Int = 2
}
