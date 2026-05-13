package gay.menkissing

import gay.menkissing.engine.*
import gay.menkissing.engine.graphics.Color

class GameState() extends GayState() {
  Textures
  def input1 = GayG.input
  val world = World.load()

  val testObject = GayText(
    """Genuinely I am fr tweaking. Its fr a problem. Help me. God hates us all, we are all sinners.
      |I am now in the process of testing newlines. Ts better work or im gonna cry
      |The lowercase 'g' seems  suspiciously broken. God hates me and Im going to end up in hell where I belong
      |This genuinely may be causing a shitton of lag. Genuinely. I sincerly hope not.
      |God isn't real
      |""".stripMargin, 30, engine.graphics.Color(1f, 1f, 1f, 1f))
  testObject.y = 50
  val testObject2 = GaySprite(GayTexture(Textures.preload.haxe))
  val testObject3 = GayMesh2D(CustomMeshes.weirdRectangleMesh, 1100, 225)
  testObject3.setLayer(GayG.initialCamera)
  testObject3.y = GayG.initialCamera.worldSize.y - 230
  testObject3.x += 240
  testObject3.setColor(Color.fromHex(0xFF2c0044))

  val bgTiles = FloorTiles(Textures.preload.tiles, 8, world.tiles.bg)
  val fgTiles = GayTiles(Textures.preload.tiles, 8, world.tiles.fg)
  bgTiles.solid = true
  fgTiles.solid = true
  fgTiles.enableAutoDepth = true

  def start(): Unit = {
    add(bgTiles)
    add(fgTiles)

    add {
      val player = new Player(input1)
      player.x = world.start.pos.x * 8
      player.y = world.start.pos.y * 8
      player
    }


    add(testObject2)
    testObject2.setLayer(GayG.initialCamera)
    add(testObject)
    testObject.setLayer(GayG.initialCamera)
    add(testObject3)
    testObject3.setLayer(GayG.initialCamera)

    world.levels(world.start.level).addEntities(this)


  }
  def clean(killPlayer: Boolean = false): Unit = {
    for (obj <- members) {
      if (killPlayer || !obj.isInstanceOf[Player])
        obj.destroy()
    }
    if (killPlayer)
      members.clear()
    else {
      val player = members.find(_.isInstanceOf[Player])
      members.clear()
      members.appendAll(player)
    }


  }


}