package gay.menkissing
package battle

import engine.group.*
import gay.menkissing.engine.GaySprite
import gay.menkissing.engine.GayTexture
import gay.menkissing.engine.GayMesh2D
import gay.menkissing.engine.graphics.GayMaterial
import gay.menkissing.engine.graphics.Texture

class BattlePortrait(val fighter: Fighter, portrait: Texture) extends GayObjectContainer {
  val portraitSprite = GaySprite(GayTexture(portrait))
  portraitSprite.scaleX = 2.0
  portraitSprite.scaleY = 2.0
  add(portraitSprite)
  val hpBar = HPBar(portraitSprite.graphicalWidth, BattlePortrait.barHeight, BattlePortrait.barPadding, fighter.maxHP)
  add(hpBar)
  hpBar.y += portraitSprite.graphicalHeight
  val spBar = HPBar(portraitSprite.graphicalWidth, BattlePortrait.barHeight, BattlePortrait.barPadding, fighter.maxSP)
  add(spBar)
  spBar.healthRect.setColor(engine.graphics.Color.fromHex(0xFFFF00FF))
  spBar.y = hpBar.y + BattlePortrait.barHeight
  val selector = GayMesh2D(CustomMeshes.triangleMesh, 100, 32)
  selector.material = GayMaterial.FlatColor(BattleMenu.primaryColor)
  selector.x += (portraitSprite.graphicalWidth - selector.graphicalWidth) / 2
  selector.y -= 10
  add(selector)
  selector.visible = false
  fighter.healthUpdateSignal.add(hpBar.setValue)
  fighter.spUpdateSignal.add(spBar.setValue)
}

object BattlePortrait {
  val barHeight: Int = 20
  val barPadding: Int = 2
}
