package gay.menkissing

import gay.menkissing.engine.Game


@main def main() =
  Game.instance.gamemanager.switchState(new GameState())
  Game.instance.gamemanager.pushState(new BattleGameState())
  Game.instance.run()
