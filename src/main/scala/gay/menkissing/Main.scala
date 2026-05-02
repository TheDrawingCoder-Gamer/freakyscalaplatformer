package gay.menkissing

import gay.menkissing.engine.Game


@main def main() =
  Game.instance.gamemanager.switchState(new BattleGameState(Game.instance.gamemanager))
  Game.instance.run()
