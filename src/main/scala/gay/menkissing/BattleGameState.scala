package gay.menkissing

import gay.menkissing.battle.BattleMenu
import gay.menkissing.engine.{GameManager, GayState, GayText}

class BattleGameState(manager: GameManager) extends GayState(manager):
  val menu: BattleMenu = BattleMenu()
  menu.cameras.hideFromDefault().addToCamera(manager.fullCam)
  
  override def start(): Unit =
    add(menu)

  override def close(): Unit = ()
