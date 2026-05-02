package gay.menkissing

import gay.menkissing.engine.{GaySprite, GayTexture}

class Destructible extends GaySprite(GayTexture(Textures.preload.destructible)) {
    hitX = 0
    hitY = 0
    hitW = 16
    hitH = 16
    solid = true

    override def canTouch(player: Player): Boolean = false
    override def touch(player: Player): Unit = ()
}
