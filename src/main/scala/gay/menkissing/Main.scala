package gay.menkissing

import gay.menkissing.engine.Game
import gay.menkissing.engine.PixelPerfectGayView
import gay.menkissing.engine.GayG


lazy val pixelCam = new PixelPerfectGayView(GraphicG.renderWidth, GraphicG.renderHeight)

@main def main() =
  val game = new Game(GraphicG.fullRenderWidth, GraphicG.fullRenderHeight)
  GayG.cameras.insert(pixelCam, 0, replaceAsDefault = true)
  GayG.switchState(new GameState())
  GayG.pushState(new BattleGameState())
  game.run()
